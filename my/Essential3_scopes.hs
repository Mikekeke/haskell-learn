fn = \s -> res where res = s*2 -- 's' not in scope
fn s = res where res = s*2 -- 's' in scope