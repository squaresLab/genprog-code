import sys
import getopt
import commands
import csv


numF = 0

def sort_dict(cbi_dict):
    return sorted(cbi_dict.items(), key= lambda t: t[1]["importance"])

def do_cbi(obs_dict):
    cbi_dict = {}

    for predicate in obs_dict.keys():
        pred_dict = obs_dict[predicate]
        if not cbi_dict.has_key(predicate):
            cbi_dict[predicate] = {}
            
        fp = float(pred_dict["fp"])
        sp = float(pred_dict["sp"])
        fobs = float(pred_dict["fobs"])
        sobs = float(pred_dict["sobs"])
        
        if fp + sp > 0:
            failurep = fp / (fp + sp)
        else:
            failurep = 0.0

        if fobs + sobs > 0:
            context = fobs / (fobs + sobs)
        else:
            context = 0.0

        increase = float(failurep - context)

        if increase > 0 and failurep > 0:
            cbi_dict[predicate]["importance"] = 2.0 / ((1.0/increase) + (float(numF)/fp))
        else:
            cbi_dict[predicate]["importance"] = 0.0

        cbi_dict[predicate]["failurep"] = failurep
        cbi_dict[predicate]["context"] = context
        cbi_dict[predicate]["increase"] = increase

    return cbi_dict

def compute_observations(run_dict, passing_runs_list):
    obs_dict = {}

    # run_dict maps states -> run -> predicate -> num
    
    for state in run_dict.keys():
        pred_list = []

        # state_dict maps run, predicate, to num observed true

        state_dict = run_dict[state]

        for run in state_dict.keys():
            pred_list = []

            # pred_dict maps predicate -> num observed true
            pred_dict = state_dict[run]

            num_obs = 0

            for predicate in pred_dict.keys():
                if not obs_dict.has_key(predicate):
                    obs_dict[predicate] = {"fp" : 0, "sp" : 0, "fobs" : 0, "sobs" : 0}

                pred_list.append(predicate)
                fp = 0
                sp = 0

                # was it observed on this run?
                val = 0
                if pred_dict[predicate] > 0:
                    val = 1

                num_obs += val
                fp = obs_dict[predicate]["fp"]
                sp = obs_dict[predicate]["sp"]

                if run in passing_runs_list:
                    sp += val
                else:
                    fp += val
                obs_dict[predicate]["fp"] = fp
                obs_dict[predicate]["sp"] = sp

            for predicate in pred_list:
                if run in passing_runs_list:
                    obs_dict[predicate]["sobs"] += num_obs
                else:
                    obs_dict[predicate]["fobs"] += num_obs

    return obs_dict

def main():

    global numF

    run_data = sys.argv[1]
    passing_runs_file = sys.argv[2]
    numF = sys.argv[3]

    run_reader = csv.reader(open(run_data), delimiter=',')
    header_row = run_reader.next()
    run_dict = {}
    passing_runs_list = []

# state,type, pred, run1_num, run2_num, run3_num ...

    state = ""
    for row in run_reader:
        predicate = "FOO"
        for colnum,col in enumerate(row):
            if colnum == 0 and (not col == ""):
                state = col
                if not run_dict.has_key(state):
                    run_dict[state] = {}
                    # 0 and 1 are state and predicate, 2 on is run numbers
                    for head in header_row[2:]:
                        run_dict[state][int(head)] = {}
            elif colnum == 1:
                predicate = col
            elif colnum > 0:
                run_dict[state][int(header_row[colnum])][state + "_" + predicate] = int(col)

    for line in open(passing_runs_file, 'r').readlines():
        if len(line.strip()) > 0:
            passing_runs_list.append(int(line.strip()))
    
    obs_dict = compute_observations(run_dict, passing_runs_list)
    cbi_dict = do_cbi(obs_dict)
    sorted_keys = sort_dict(cbi_dict)

    print "cbi_dict:"
    print cbi_dict
    for predicate in sorted_keys:
        print "P: " + predicate[0] + ": "
        print "     failurep -> " + str(predicate[1]["failurep"])
        print "     context -> " + str(predicate[1]["context"])
        print "     increase -> " + str(predicate[1]["increase"])
        print "     importance -> " + str(predicate[1]["importance"])


main()
