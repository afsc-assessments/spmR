from pathlib import Path
import argparse, csv, json, math, re, shutil, statistics, struct, subprocess
ROOT=Path(__file__).resolve().parent
EXE=ROOT/'spm'

def write_pairs(path, pairs):
    path.write_text(''.join('# '+k+'\n'+(' '.join(map(str,v)) if isinstance(v,list) else str(v))+'\n' for k,v in pairs))

def fixture(name, nsex=2, basis=2, nsims=5000, mode=1, pop_f=1, pop_m=1, constant=False):
    d=ROOT/'fixtures'/name;d.mkdir(parents=True,exist_ok=True)
    age=15;n=[100*.5**a for a in range(age)];n[-1]/=.5
    rec=[80,120]*15 if not constant else [100]*30
    if basis==1:rec=[2*x for x in rec]
    ssb=[200]*30
    if mode==2:
        ssb=[20+13*k for k in range(30)]
        # Positive known Beverton-Holt total-recruitment data with paired deviations.
        rec=[s/(1/12+(2.75/1200)*s)*math.exp([-.20,.20][k%2]-.02) for k,s in enumerate(ssb)]
        if basis==2:rec=[x/2 for x in rec]
    p=[('spname','audit'),('SSL',0),('buffer',0),('ngear',1),('nsex',nsex),('avgF',.2),('adjust',1),('sprabc',.4),('sprofl',.35),('spawnmonth',1),('nages',age),('Fratio',1),('M_F',[math.log(2)]*age)]
    if nsex==2:p.append(('M_M',[math.log(2)]*age))
    p.append(('matureF',[1]*age))
    if nsex==2:p.append(('matureM',[1]*age))
    p.append(('spawnF',[1]*age))
    if nsex==2:p.append(('spawnM',[1]*age))
    p.append(('fishF',[1]*age))
    if nsex==2:p.append(('fishM',[1]*age))
    p.append(('selF',[1]*age))
    if nsex==2:p.append(('selM',[1]*age))
    p.append(('N_F',n if nsex==2 else [2*x for x in n]))
    if nsex==2:p.append(('N_M',n))
    p.extend([('nrec',30),('R',rec),('SSB',ssb)])
    write_pairs(d/'audit.prj',p)
    controls=[('run','audit'),('Tier',3),('nalts',1),('alts',5),('tac',1),('SrType',2),('Rec_Gen',mode),('Fmsy',0),('Rec_Cond',0),('detail',1),('npro',3),('nsims',nsims),('start',2026),('nyrscatch',1),('nspp',1),('OYmin',0),('OYmax',2e6),('filename','audit.prj'),('ABCmult',1),('Nscalar',1),('alt4spr',.6),('ntacspp',1),('tacind',1),('Catch',[2026,0])]
    write_pairs(d/'spm.dat',controls)
    meta=['SPMR_INPUT_V2 2 1',f'audit.prj {nsex} {age} {basis} 1 1 1 0',' '.join(map(str,range(1,age+1))),' '.join([str(pop_f)]*age),' '.join([str(pop_m)]*age),'END_SPMR_INPUT_V2']
    (d/'spm_input_v2.dat').write_text('\n'.join(meta)+'\n')
    return d

def run(d,good=True,expected_stocks=1):
    for fn in ['spm_detail.csv','spm_input_receipt.tsv']:
        (d/fn).unlink(missing_ok=True)
    r=subprocess.run([str(EXE),'-nolp'],cwd=d,stdout=subprocess.PIPE,stderr=subprocess.STDOUT,timeout=120)
    (d/'run.log').write_bytes(r.stdout)
    if not good:
        assert r.returncode==2,(d.name,r.returncode,r.stdout[-2000:])
        assert b'SPMR input format 2:' in r.stdout,(d.name,r.stdout[-2000:])
        assert not (d/'spm_detail.csv').exists() or (d/'spm_detail.csv').stat().st_size==0,d.name
        return {'exit_code':r.returncode,'diagnostic':[s for s in r.stdout.decode().splitlines() if 'SPMR input format 2:' in s]}
    assert r.returncode==0,(d.name,r.returncode,r.stdout[-2000:])
    assert b'Finished simulations using' in r.stdout,(d.name,r.stdout[-2000:])
    rows=list(csv.DictReader((d/'spm_detail.csv').open()))
    assert rows,d.name
    assert all(math.isfinite(float(row[k])) for row in rows for k in row if k!='Stock'),d.name
    rec=list(csv.DictReader((d/'spm_input_receipt.tsv').open(),delimiter='\t'))
    assert len(rec)==expected_stocks and all(record['format_version']=='2' for record in rec),d.name
    return {'exit_code':r.returncode,'rows':len(rows),'receipt':rec[0],'receipt_count':len(rec),'first':rows[0],'mean_rec':statistics.mean(float(x['Rec']) for x in rows),'cv_rec':statistics.pstdev(float(x['Rec']) for x in rows)/statistics.mean(float(x['Rec']) for x in rows)}

def inline_metadata(directory,keep_sidecar=False,selected_files=None):
    sidecar=directory/'spm_input_v2.dat'
    metadata=sidecar.read_text().splitlines()
    stock_count=int(metadata[0].split()[2])
    for stock_index in range(stock_count):
        stock_lines=metadata[1+4*stock_index:5+4*stock_index]
        filename=stock_lines[0].split()[0]
        if selected_files is not None and filename not in selected_files:
            continue
        species_path=directory/filename
        block=['SPMR_INPUT_V2 2 1']+stock_lines+['END_SPMR_INPUT_V2']
        species_path.write_text(species_path.read_text()+'\n'+''.join('# '+line+'\n' for line in block))
    if not keep_sidecar:
        sidecar.unlink()

def multi_stock_fixture(name):
    directory=fixture(name,nsims=10)
    species=directory/'audit.prj'
    (directory/'other.prj').write_text(species.read_text().replace('# spname\naudit\n','# spname\nother\n'))
    controls=directory/'spm.dat'
    text=controls.read_text()
    changes={'# nspp\n1\n':'# nspp\n2\n',
      '# filename\naudit.prj\n':'# filename\naudit.prj other.prj\n',
      '# ABCmult\n1\n':'# ABCmult\n1 1\n',
      '# Nscalar\n1\n':'# Nscalar\n1 1\n',
      '# alt4spr\n0.6\n':'# alt4spr\n0.6 0.6\n',
      '# ntacspp\n1\n':'# ntacspp\n2\n',
      '# tacind\n1\n':'# tacind\n1 2\n',
      '# Catch\n2026 0\n':'# Catch\n2026 40 40\n'}
    for before,after in changes.items():
        assert before in text,before
        text=text.replace(before,after)
    controls.write_text(text)
    sidecar=directory/'spm_input_v2.dat'
    metadata=sidecar.read_text().splitlines()
    second_stock=[line.replace('audit.prj','other.prj') for line in metadata[1:5]]
    sidecar.write_text('\n'.join(['SPMR_INPUT_V2 2 2']+metadata[1:5]+second_stock+['END_SPMR_INPUT_V2'])+'\n')
    return directory

def weight_fixture(name, fish_multiplier=1., male_pop_multiplier=1., male_spawn_multiplier=1.):
    d=fixture(name,nsims=1)
    p=d/'audit.prj';text=p.read_text()
    nages=15
    female=[90*.75**a for a in range(nages)];female[-1]/=.25
    male=[110*.7**a for a in range(nages)];male[-1]/=.3
    inputs={'N_F':female,'N_M':male,
      'M_F':[.2+.01*a for a in range(nages)],'M_M':[.3+.015*a for a in range(nages)],
      'selF':[min(.1+.1*a,1.) for a in range(nages)],'selM':[min(.05+.075*a,.8) for a in range(nages)],
      'matureF':[0,.25,.6]+[1]*12,'matureM':[0,.4,.8]+[1]*12,
      'spawnF':[.5+.1*a for a in range(nages)],
      'spawnM':[male_spawn_multiplier*(.35+.08*a) for a in range(nages)],
      'fishF':[fish_multiplier*(.2+.04*a) for a in range(nages)],
      'fishM':[fish_multiplier*(.15+.025*a) for a in range(nages)],
      'popF':[.3+.15*a for a in range(nages)],
      'popM':[male_pop_multiplier*(.25+.11*a) for a in range(nages)]}
    for field,values in inputs.items():
        if field.startswith('pop'):continue
        text=re.sub(r'(# '+field+r'\n)[^\n]+',lambda m:m.group(1)+' '.join(map(str,values)),text)
    p.write_text(text)
    p=d/'spm.dat';p.write_text(p.read_text().replace('# Catch\n2026 0\n','# Catch\n2026 40\n'))
    p=d/'spm_input_v2.dat';lines=p.read_text().splitlines();lines[3]=' '.join(map(str,inputs['popF']));lines[4]=' '.join(map(str,inputs['popM']));p.write_text('\n'.join(lines)+'\n')
    return d,inputs

def check_weight_trajectory(d,inputs):
    # Each row reports current-year biomass; Rec is total recruitment entering
    # the following projection year. Both sexes receive exactly half that draw.
    rows=list(csv.DictReader((d/'spm_detail.csv').open()))
    female=inputs['N_F'][:];male=inputs['N_M'][:];errors=[]
    for row in rows:
        f=float(row['F']);rec=float(row['Rec'])
        expected_biomass=sum(n*w for n,w in zip(female,inputs['popF']))+sum(n*w for n,w in zip(male,inputs['popM']))
        expected_ssb=sum(n*m*w for n,m,w in zip(female,inputs['matureF'],inputs['spawnF']))
        actual_biomass=float(row['Tot_biom']);actual_ssb=float(row['SSB'])
        assert math.isclose(actual_biomass,expected_biomass,rel_tol=2e-5,abs_tol=.002),(d,row,expected_biomass)
        assert math.isclose(actual_ssb,expected_ssb,rel_tol=2e-5,abs_tol=.002),(d,row,expected_ssb)
        expected_catch=0.;next_counts=[]
        for numbers,sex in [(female,'F'),(male,'M')]:
            mortality=[m+f*sel for m,sel in zip(inputs['M_'+sex],inputs['sel'+sex])]
            survival=[math.exp(-z) for z in mortality]
            expected_catch+=sum(n*w*f*sel/z*(1-surv) for n,w,sel,z,surv in zip(numbers,inputs['fish'+sex],inputs['sel'+sex],mortality,survival))
            next_n=[rec/2]+[n*surv for n,surv in zip(numbers[:-1],survival[:-1])]
            next_n[-1]+=numbers[-1]*survival[-1]
            next_counts.append(next_n)
        assert math.isclose(float(row['Catch']),expected_catch,rel_tol=2e-5,abs_tol=.002),(d,row,expected_catch)
        errors.append({'year':int(row['Year']),'expected_biomass':expected_biomass,'reported_biomass':actual_biomass,'biomass_error':actual_biomass-expected_biomass,'ssb_error':actual_ssb-expected_ssb,'catch_error':float(row['Catch'])-expected_catch})
        female,male=next_counts
    return errors

def check_sr_fit(d):
    par=(d/'spm.par').read_text()
    grad=float(re.search(r'Maximum gradient component = ([^\s]+)',par).group(1))
    assert math.isfinite(grad) and grad<1e-4,(d,grad)
    raw=(d/'admodel.hes').read_bytes()
    # ADMB writes a native-endian int dimension followed by row-major doubles.
    n=struct.unpack('=i',raw[:4])[0]
    assert 0<n<1000 and len(raw)>=4+8*n*n
    values=struct.unpack('='+str(n*n)+'d',raw[4:4+8*n*n])
    h=[list(values[i*n:(i+1)*n]) for i in range(n)]
    assert all(math.isfinite(x) for x in values)
    max_asym=max(abs(h[i][j]-h[j][i]) for i in range(n) for j in range(n))
    assert max_asym<1e-5
    # Independent Cholesky decomposition checks positive definiteness.
    lower=[[0.]*n for _ in range(n)]
    for i in range(n):
        for j in range(i+1):
            value=.5*(h[i][j]+h[j][i])-sum(lower[i][k]*lower[j][k] for k in range(j))
            if i==j:
                assert value>0,(d,i,value)
                lower[i][j]=math.sqrt(value)
            else:lower[i][j]=value/lower[j][j]
    return {'maximum_gradient':grad,'hessian_positive_definite':True,'hessian_max_asymmetry':max_asym,'cholesky_diagonal':[lower[i][i] for i in range(n)]}

if __name__=='__main__':
    parser=argparse.ArgumentParser(description='Synthetic native SPM v2 validation; no assessment data required.')
    parser.add_argument('--executable',type=Path,required=True)
    parser.add_argument('--output',type=Path,required=True)
    args=parser.parse_args()
    EXE=args.executable.resolve();ROOT=args.output.resolve();ROOT.mkdir(parents=True,exist_ok=True)
    results={}
    r=subprocess.run([str(EXE),'-spmr-capabilities'],cwd=ROOT,stdout=subprocess.PIPE,stderr=subprocess.PIPE)
    assert r.returncode==0 and r.stdout==b'SPMR_INPUT_FORMAT=2\nSPMR_INLINE_INPUT=1\n' and r.stderr==b''
    for name,nsex,basis in [('split_per_sex',2,2),('split_total',2,1),('pooled_total',1,1)]:
        results[name]=run(fixture(name,nsex,basis))
    expected=(ROOT/'fixtures/split_total/spm_detail.csv').read_bytes()
    for name in ['split_per_sex','pooled_total']:
        assert expected==(ROOT/'fixtures'/name/'spm_detail.csv').read_bytes(),name
    for name,nsex,basis in [('inline_per_sex',2,2),('inline_total',2,1),('inline_pooled',1,1)]:
        directory=fixture(name,nsex,basis)
        inline_metadata(directory)
        results[name]=run(directory)
        assert expected==(directory/'spm_detail.csv').read_bytes(),name
    directory=fixture('inline_stale_sidecar',basis=1)
    inline_metadata(directory,keep_sidecar=True)
    (directory/'spm_input_v2.dat').write_text('stale invalid metadata\n')
    results['inline_stale_sidecar']=run(directory)
    assert expected==(directory/'spm_detail.csv').read_bytes()
    directory=multi_stock_fixture('multi_sidecar')
    results['multi_sidecar']=run(directory,expected_stocks=2)
    multi_expected=(directory/'spm_detail.csv').read_bytes()
    directory=multi_stock_fixture('multi_inline')
    inline_metadata(directory)
    results['multi_inline']=run(directory,expected_stocks=2)
    assert multi_expected==(directory/'spm_detail.csv').read_bytes()
    # Status-determination catches remain specific to each stock, including
    # after alternative 7 switches from maximum ABC to its status ramp.
    for alternative in [6,7]:
        name='multi_status_'+str(alternative)
        directory=multi_stock_fixture(name)
        controls=directory/'spm.dat'
        controls.write_text(controls.read_text().replace('# alts\n5\n','# alts\n'+str(alternative)+'\n').replace('# nsims\n10\n','# nsims\n1\n').replace('# npro\n3\n','# npro\n5\n'))
        species=directory/'other.prj'
        text=species.read_text()
        for field in ['N_F','N_M','R','SSB']:
            text=re.sub(r'(# '+field+r'\n)([^\n]+)',lambda m:m.group(1)+' '.join(str(float(x)*2) for x in m.group(2).split()),text)
        species.write_text(text)
        results[name]=run(directory,expected_stocks=2)
        rows=list(csv.DictReader((directory/'spm_detail.csv').open()))
        for row in rows:
            year=int(row['Year'])
            if year==2026:
                assert float(row['Catch'])==40,(name,row)
                continue
            expected_field='OFL' if alternative==6 else ('MaxABC' if year<=2028 else 'ABC')
            assert math.isclose(float(row['Catch']),float(row[expected_field]),rel_tol=1e-5,abs_tol=.002),(name,expected_field,row)
        second_year={row['Stock']:float(row['Catch']) for row in rows if row['Year']=='2027'}
        assert second_year['audit']!=second_year['other'],(name,second_year)
    directory=multi_stock_fixture('invalid_inline_mixed')
    inline_metadata(directory,keep_sidecar=True,selected_files=['audit.prj'])
    results['invalid_inline_mixed']=run(directory,False)
    assert abs(results['split_total']['cv_rec']-math.sqrt(100/96-1))<.01
    for name,kw in [('zero_variance',{'constant':True}),('population_weights',{'nsims':1,'pop_f':2,'pop_m':3}),('deterministic',{'nsims':1})]:
        results[name]=run(fixture(name,**kw))
    assert results['zero_variance']['mean_rec']==200 and results['zero_variance']['cv_rec']==0
    assert float(results['population_weights']['first']['Tot_biom'])==1000
    assert float(results['population_weights']['first']['SSB'])==200
    assert float(results['deterministic']['first']['Rec'])==200
    assert float(results['deterministic']['first']['B40'])==80
    # Invalid sidecar and unsupported model modes fail before producing projections.
    mutations={
      'missing':lambda s:None,
      'truncated':lambda s:'\n'.join(s.splitlines()[:-2]),
      'trailing':lambda s:s+'extra\n',
      'bad_version':lambda s:s.replace('SPMR_INPUT_V2 2 1','SPMR_INPUT_V2 1 1'),
      'bad_count':lambda s:s.replace('SPMR_INPUT_V2 2 1','SPMR_INPUT_V2 2 2'),
      'wrong_file':lambda s:s.replace('audit.prj','other.prj'),
      'wrong_sexes':lambda s:s.replace('audit.prj 2 15 2','audit.prj 1 15 1'),
      'wrong_ages':lambda s:s.replace('audit.prj 2 15 2','audit.prj 2 14 2'),
      'wrong_basis':lambda s:s.replace('audit.prj 2 15 2','audit.prj 2 15 7'),
      'bad_age_vector':lambda s:s.replace('1 2 3 4 5','1 2 4 4 5'),
      'zero_weight':lambda s:s.replace('\n1 1 1 1 1','\n0 1 1 1 1',1),
      'bad_units':lambda s:s.replace('2 15 2 1 1 1 0','2 15 2 2 1 1 0'),
      'bad_weight_unit':lambda s:s.replace('2 15 2 1 1 1 0','2 15 2 1 2 1 0'),
      'bad_substitute':lambda s:s.replace('2 15 2 1 1 1 0','2 15 2 1 1 1 3'),
    }
    for storage in ['sidecar','inline']:
        case='preserve_outputs_invalid_'+storage
        directory=fixture(case,nsims=1)
        sidecar=directory/'spm_input_v2.dat'
        sidecar.write_text(mutations['bad_units'](sidecar.read_text()))
        if storage=='inline':
            inline_metadata(directory)
        saved_outputs={filename:('preserve '+filename+'\n').encode() for filename in [
          'spm_detail.csv','spm_summary.csv','means.out','alt_proj.out',
          'percentiles.out','F_profile.out','elasticity.csv']}
        for filename,contents in saved_outputs.items():
            (directory/filename).write_bytes(contents)
        outcome=subprocess.run([str(EXE),'-nolp'],cwd=directory,stdout=subprocess.PIPE,stderr=subprocess.STDOUT,timeout=120)
        (directory/'run.log').write_bytes(outcome.stdout)
        assert outcome.returncode==2,(case,outcome.returncode,outcome.stdout[-2000:])
        assert b'SPMR input format 2: incompatible units' in outcome.stdout,(case,outcome.stdout[-2000:])
        for filename,contents in saved_outputs.items():
            assert (directory/filename).read_bytes()==contents,(case,filename)
        results[case]={'exit_code':outcome.returncode,'preserved_outputs':list(saved_outputs)}
    for name,mutate in mutations.items():
        d=fixture('invalid_'+name,nsims=1);p=d/'spm_input_v2.dat';new=mutate(p.read_text())
        if new is None:p.unlink()
        else:p.write_text(new)
        results['invalid_'+name]=run(d,False)
    for name in ['truncated','trailing','bad_version','bad_count','wrong_file','wrong_sexes','wrong_ages','wrong_basis','bad_age_vector','zero_weight','bad_units','bad_weight_unit','bad_substitute']:
        directory=fixture('invalid_inline_'+name,nsims=1)
        sidecar=directory/'spm_input_v2.dat'
        block=mutations[name](sidecar.read_text())
        if name=='trailing':
            block=block.replace('END_SPMR_INPUT_V2\nextra','END_SPMR_INPUT_V2 extra')
        species=directory/'audit.prj'
        species.write_text(species.read_text()+''.join('# '+line+'\n' for line in block.splitlines()))
        sidecar.unlink()
        results['invalid_inline_'+name]=run(directory,False)
    block_mutations={
      'duplicate':lambda block:block+block,
      'uncommented':lambda block:block.replace('# SPMR_INPUT_V2','SPMR_INPUT_V2',1),
      'broken_comment':lambda block:block.replace('# audit.prj','audit.prj',1),
      'extra_sentinel':lambda block:block+'# END_SPMR_INPUT_V2\n',
      'orphan_sentinel':lambda block:'# END_SPMR_INPUT_V2\n',
    }
    for name,mutate in block_mutations.items():
        directory=fixture('invalid_inline_'+name,nsims=1)
        sidecar=directory/'spm_input_v2.dat'
        block=''.join('# '+line+'\n' for line in sidecar.read_text().splitlines())
        species=directory/'audit.prj'
        species.write_text(species.read_text()+mutate(block))
        sidecar.unlink()
        results['invalid_inline_'+name]=run(directory,False)
    for mode in [3,4]:results['invalid_mode'+str(mode)]=run(fixture('invalid_mode'+str(mode),mode=mode,nsims=1),False)
    d=fixture('invalid_scalar',nsims=1);p=d/'spm.dat';p.write_text(p.read_text().replace('# Nscalar\n1\n','# Nscalar\n1000\n'));results['invalid_scalar']=run(d,False)
    for name,nsex,basis in [('sr2_split',2,2),('sr2_total',2,1),('sr2_pooled',1,1)]:results[name]=run(fixture(name,nsex,basis,nsims=10,mode=2))
    sr=(ROOT/'fixtures/sr2_total/spm_detail.csv').read_bytes()
    for name in ['sr2_split','sr2_pooled']:assert sr==(ROOT/'fixtures'/name/'spm_detail.csv').read_bytes(),name
    for name in ['sr2_split','sr2_total','sr2_pooled']:
        results[name]['convergence']=check_sr_fit(ROOT/'fixtures'/name)
    # Reordering independent Ricker stocks must preserve their projections.
    directory=multi_stock_fixture('ricker_order_forward')
    controls=directory/'spm.dat'
    controls.write_text(controls.read_text().replace('# SrType\n2\n','# SrType\n4\n').replace('# Rec_Gen\n1\n','# Rec_Gen\n2\n'))
    for filename,mortality,rzero,steepness in [('audit.prj',math.log(2),500,.65),('other.prj',.4,700,.8)]:
        bzero=.5*rzero/(1-math.exp(-mortality))
        beta=math.log(5*steepness)/(.8*bzero)
        alpha=math.log(rzero/bzero)+beta*bzero
        ssb=[bzero*(.1+1.2*k/29) for k in range(30)]
        recruitment=[s*math.exp(alpha-beta*s+[-.2,.2][k%2]-.02)/2 for k,s in enumerate(ssb)]
        species=directory/filename
        text=species.read_text()
        for field,values in [('M_F',[mortality]*15),('M_M',[mortality]*15),('R',recruitment),('SSB',ssb)]:
            text=re.sub(r'(# '+field+r'\n)([^\n]+)',lambda m:m.group(1)+' '.join(map(str,values)),text)
        species.write_text(text)
    reverse=directory.parent/'ricker_order_reverse'
    shutil.copytree(directory,reverse,dirs_exist_ok=True)
    controls=reverse/'spm.dat'
    controls.write_text(controls.read_text().replace('audit.prj other.prj','other.prj audit.prj'))
    sidecar=reverse/'spm_input_v2.dat'
    lines=sidecar.read_text().splitlines()
    sidecar.write_text('\n'.join(lines[:1]+lines[5:9]+lines[1:5]+lines[9:])+'\n')
    for candidate in [directory,reverse]:
        results[candidate.name]=run(candidate,expected_stocks=2)
        results[candidate.name]['convergence']=check_sr_fit(candidate)
    forward_rows={(r['Stock'],r['Sim'],r['Year']):r for r in csv.DictReader((directory/'spm_detail.csv').open())}
    reverse_rows={(r['Stock'],r['Sim'],r['Year']):r for r in csv.DictReader((reverse/'spm_detail.csv').open())}
    assert forward_rows.keys()==reverse_rows.keys()
    for key,row in forward_rows.items():
        for field in row:
            if field=='Stock':continue
            assert math.isclose(float(row[field]),float(reverse_rows[key][field]),rel_tol=2e-5,abs_tol=.002),(key,field,row[field],reverse_rows[key][field])
    # The same physical population in fish/kg, thousand fish/t, and million fish/thousand t.
    # Recruitment and all biomass/catch inputs scale together; individual weights stay in kg.
    reference_dir=ROOT/'fixtures/sr2_total'
    reference_par=(reference_dir/'spm.par').read_text()
    def fitted(text,name):
        return float(re.search(r'# '+name+r':\s*([^\s]+)',text).group(1))
    def objective(text):
        return float(re.search(r'Objective function value = ([^\s]+)',text).group(1))
    reference_rows=list(csv.DictReader((reference_dir/'spm_detail.csv').open()))
    scaled_columns=['SSB','Rec','Tot_biom','Ntot','Catch','ABC','OFL','B100','B40','B35','MaxABC']
    for unit_code,scale in [(2,1e-3),(3,1e-6)]:
        name='sr2_units_'+str(unit_code)
        d=fixture(name,basis=1,nsims=10,mode=2)
        p=d/'audit.prj';text=p.read_text()
        for field in ['N_F','N_M','R','SSB']:
            text=re.sub(r'(# '+field+r'\n)([^\n]+)',lambda m:m.group(1)+' '.join(str(float(x)*scale) for x in m.group(2).split()),text)
        p.write_text(text)
        p=d/'spm_input_v2.dat';p.write_text(p.read_text().replace('2 15 1 1 1 1 0',f'2 15 1 {unit_code} 1 {unit_code} 0'))
        results[name]=run(d);results[name]['convergence']=check_sr_fit(d)
        parameters=(d/'spm.par').read_text()
        objective_difference=objective(parameters)-objective(reference_par)
        assert abs(objective_difference)<1e-7,(name,objective_difference)
        differences={key:fitted(parameters,key)-fitted(reference_par,key) for key in ['log_Rzero','steepness','sigr']}
        differences['log_Rzero']-=math.log(scale)
        assert max(map(abs,differences.values()))<1e-6,(name,differences)
        max_relative_difference=0.
        rows=list(csv.DictReader((d/'spm_detail.csv').open()))
        for ref,row in zip(reference_rows,rows):
            for field in scaled_columns:
                wanted=float(ref[field]);actual=float(row[field])/scale
                assert math.isclose(actual,wanted,rel_tol=2e-5,abs_tol=.002),(name,field,actual,wanted)
                if wanted!=0:max_relative_difference=max(max_relative_difference,abs(actual/wanted-1))
            for field in ['F','SPR_Implied','SexRatio']:
                assert math.isclose(float(row[field]),float(ref[field]),rel_tol=2e-5,abs_tol=1e-8)
        results[name]['unit_equivalence']={'objective_difference':objective_difference,'parameter_differences_after_unit_conversion':differences,'max_relative_projection_difference':max_relative_difference}
    # Female maturity starts at the second modeled age so SPR40 is attainable.
    # Male fishing selectivity differs; population weights stay separate from catch weights.
    d=fixture('unequal_sex_reference',nsims=1,pop_f=2,pop_m=3)
    p=d/'audit.prj';text=p.read_text()
    text=text.replace('# matureF\n'+' '.join(['1']*15),'# matureF\n0 '+' '.join(['1']*14))
    text=text.replace('# selM\n'+' '.join(['1']*15),'# selM\n'+' '.join(['0.2']*15))
    p.write_text(text)
    results['unequal_sex_reference']=run(d)
    summary={row['variable']:float(row['value']) for row in csv.DictReader((d/'spm_summary.csv').open())}
    fabc=summary['F_abc']
    assert abs(fabc-math.log(7/4))<1e-5,fabc
    expected_reference=100*(2/(1-.5*math.exp(-fabc))+3/(1-.5*math.exp(-.2*fabc)))
    assert abs(summary['Total_biomass_abc']-expected_reference)<.005,(summary,expected_reference)
    assert abs(summary['SSB_40']-40)<.005,summary
    assert summary['Mean_rec']==200 and summary['Mean_rec_female']==100
    results['unequal_sex_reference']['expected_total_biomass_abc']=expected_reference
    results['unequal_sex_reference']['actual_total_biomass_abc']=summary['Total_biomass_abc']
    d=fixture('invalid_TAC',nsims=1);p=d/'spm.dat';p.write_text(p.read_text().replace('# tac\n1\n','# tac\n0\n'));results['invalid_TAC']=run(d,False)
    for name,fish,pop,spawn_m in [('weight_trajectory',1.,1.,1.),('fishery_weights_doubled',2.,1.,1.),('male_population_weights_changed',1.,1.7,1.),('male_spawning_weights_changed',1.,1.,1.8)]:
        d,inputs=weight_fixture(name,fish,pop,spawn_m)
        results[name]=run(d)
        results[name]['independent_trajectory']=check_weight_trajectory(d,inputs)
    original=results['weight_trajectory']['first'];fish_changed=results['fishery_weights_doubled']['first'];pop_changed=results['male_population_weights_changed']['first'];spawn_changed=results['male_spawning_weights_changed']['first']
    assert original['Tot_biom']==fish_changed['Tot_biom']
    assert float(original['F'])>float(fish_changed['F'])>0
    assert original['F']==pop_changed['F']
    assert float(pop_changed['Tot_biom'])>float(original['Tot_biom'])
    assert original['SSB']==spawn_changed['SSB']
    assert original['Tot_biom']==spawn_changed['Tot_biom']
    (ROOT/'validation.json').write_text(json.dumps({'status':'passed','checks':results},indent=2)+'\n')
    print(json.dumps({'status':'passed','cases':len(results),'split_equivalence':'exact CSV match across all 15000 rows','inline_equivalence':'exact CSV match for split, pooled and two-stock runs','inline_precedence':'stale sidecar ignored when every stock has inline metadata','sr2_equivalence':'exact CSV match','zero_cv':'pass','missing_invalid_metadata':'rejected'},indent=2))
