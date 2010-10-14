import sys
import commands

num_rand = int(sys.argv[2])

def get_results(debug_fname,fout):
    output = commands.getoutput("cat " + debug_fname + " | grep \"Repair Found\" | wc -l")
    tcq = int(commands.getoutput("cat " + debug_fname + " | grep \"Variant Test Case Queries\"").split(" ")[5].strip())
    tse = float(commands.getoutput("cat " + debug_fname + " | grep \"Test Suite Evaluations\"").split(" ")[5].strip())
    return int(output) > 0, tcq, tse

def ret_schemes ():
    fault_locals = ["context","failure","importance","increase","intersection","uniform"]
    for i in range(1,num_rand+1):
        fault_locals.append("random"+str(i))
    return fault_locals

def path_weight(weight_file):
    fin = open(weight_file,"r")
    weight = 0.0
    for line in fin.readlines():
        split=line.split(",")
        weight += float(split[1].strip())
    return weight

def fname(path,search,fault_local,fix,mut,seed):
    return path + "/" + search +"/debug/debug__fault_"+fault_local +"__fix_"+fix+"__mut"+mut+"__seed"+str(seed)+".debug"

def one_program(progname,path,brute_fout,ga_fout, weights_fout):
    brute_fout.write("BENCHMARK: " + progname + "\n")
    ga_fout.write("BENCHMARK: " + progname + "\n")

    for fault_local in ret_schemes():
        weight_file = prog_name+"-"+fault_local"-fault_local.txt"
        weight = path_weight(weight_file)
        weights_fout.write(fault_local+","+str(weight))
        path += "/results/"
        # get path weight for this scheme
        for fix in ["default","uniform"]:
            fname = fname(path,"brute",fault_local,fix,"0.01",0)
            found,tcq,tse=get_results(fname,brute_fout)
            brute_fout.write(fault_local+","+fix+","+str(tcq)+","+str(tse)+"\n")

            total_found = 0.0
            total_tcq = 0.0
            total_tse = 0.0

            for seed in range(100):
                mut = "0.01"
                fname = fname(path,"ga",fault_local,fix,mut,seed)
                found,tcq1,tse1=get_results(fname)
                if found:
                    total_found += 1.0
                    total_tcq += tcq1
                    total_tse += tse1
                else: 
                    mut = "0.06"
                    fname = fname(path,"ga",fault_local,fix,mut,seed)
                    found,tcq2,tse2=get_results(fname)
                    if found:
                        total_found += 1.0
                        total_tcq += tcq2 + tcq1
                        total_tse += tse2 + tse1
            total_tcq /= total_found
            total_tse /= total_found
            total_found /= 100.0
            ga_fout.write(fault_local+"," + fix+"," + str(total_found) + "," + str(total_tcq) + "," + str(total_tse) + "\n")

def main():
    fin = open(sys.argv[1],"r")
    benches = fin.readlines()
    for bench in benches:
        bench = bench.strip()
        brute_fout_fname = bench + "_brute_force.csv"
        brute_fout = open(brute_fout_fname, "w")
        ga_fout_fname = bench + "_ga.csv"
        ga_fout = open(ga_fout_fname, "w")
        weights_fout_fname = bench + "_weights.csv"
        weights_fout = open(weights_fout_fname, "w")

        brute_fout.write("fault localization scheme,fix localization scheme,TCQ,TSE\n")
        ga_fout.write("fault localization scheme,fix localization scheme,success rate,TCQ,TSE\n")
        weights_fout.write("fault localizations scheme, path weight\n")
        one_program(bench, "./"+bench, brute_fout, ga_fout, weights_fout)
        close(brute_fout)
        close(ga_fout)
        close(weights_fout)

    close(fin)

main()
