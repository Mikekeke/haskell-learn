fn1 = \s -> res where res = s*2 -- 's' not in scope
fn2 s = res where res = s*2 -- 's' in scope
