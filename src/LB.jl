module LB

command = """
open imp

transaction

exec '^cost["Mango Sorbet"] = 40.
^cost["Cone Chocolate"] = 52.
^cost["Cone Vanilla"] = 44.
^cost["Code Chi Fries"] = 200.'

query '_(i, c) <- cost(i, c).'   

query '_(i) <- cost(i, c).'

commit
"""

function lb(command)
  write("/tmp/script.lb", command)
  output = readstring(`/home/jamie/logicblox-x86_64-linux-4.3.17-a3f4888aecd21c326197b02deae87d1d74569cc5/bin/lb /tmp/script.lb`)
  raw_results = matchall(r"/--------------- _ ---------------\\([^$\\]*)\\--------------- _ ---------------/"s, output)
  results = [readdlm(IOBuffer(raw_result[37:(length(raw_result)-36)])) for raw_result in raw_results]
end

output = lb(command)

end
