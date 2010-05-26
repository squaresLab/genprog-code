import sys
import csv

numF = 0

def sort_dict(cbi_dict):
    return sorted(cbi_dict.items(), key= lambda t: t[1]["importance"])

def do_probs(obs_dict):
    prob_dict = {}

    for predicate in obs_dict.keys():
        prob_true = 0.0
        prob_false = 0.0
        pTP = 0.0
        pFP = 0.0
        pTF = 0.0
        pFF = 0.0

        pred_dict = obs_dict[predicate]
        if not prob_dict.has_key(predicate):
            prob_dict[predicate] = {}

        nPT = float(pred_dict["nPT"])
        nFT = float(pred_dict["nFT"])
        nPO = float(pred_dict["nPO"])
        nFO = float(pred_dict["nFO"])

        total_true = nPT + nFT
        total_false = (nPO - nPT) + (nFO - nFT)
        total_obs = nPO + nFO

        if total_obs > 0:
            prob_true = total_true / total_obs
            prob_false = total_false / total_obs
        
        if nPO > 0:
            pTP = nPT / nPO
        
        pFP = 1.0 - pTP

        if nFO > 0:
            pTF = nFT / nFO

        pFF = 1.0 - pTF
        prob_dict[predicate] = {"pT" : prob_true, "pF" : prob_false, "pTP" : pTP, "pFP" : pFP, "pTF" : pTF, "pFF" : pFF}
    return prob_dict

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

            total_num_obs = 0
            for predicate in pred_dict.keys():
                if not obs_dict.has_key(predicate):
                    obs_dict[predicate] = {"fp" : 0, "sp" : 0, "fobs" : 0, "sobs" : 0, "nPT" : 0, "nFT": 0, "nPO" : 0, "nFO" : 0}

                pred_list.append(predicate)

                # was it observed on this run?
                val = 0
                if pred_dict[predicate] > 0:
                    val = 1

                num_obs += val
                total_num_obs += pred_dict[predicate]

                if run in passing_runs_list:
                    obs_dict[predicate]["sp"] += val
                    obs_dict[predicate]["nPT"] += pred_dict[predicate]
                else:
                    obs_dict[predicate]["fp"] += val
                    obs_dict[predicate]["nFT"] += pred_dict[predicate]


            for predicate in pred_list:
                if run in passing_runs_list:
                    obs_dict[predicate]["sobs"] += num_obs
                    obs_dict[predicate]["nPO"] += total_num_obs
                else:
                    obs_dict[predicate]["fobs"] += num_obs
                    obs_dict[predicate]["nFO"] += total_num_obs

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
    prob_dict = do_probs(obs_dict)
    sorted_keys = sort_dict(cbi_dict)

    for predicate in sorted_keys:
        print "P: " + predicate[0] + ": "
        print "    probability T -> " + str(prob_dict[predicate[0]]["pT"])
        print "    probability F -> " + str(prob_dict[predicate[0]]["pF"])
        print "    probability T on passing runs -> " + str(prob_dict[predicate[0]]["pTP"])
        print "    probability F on passing runs -> " + str(prob_dict[predicate[0]]["pFP"])
        print "    probability T on failing runs -> " + str(prob_dict[predicate[0]]["pTF"])
        print "    probability F on failing runs -> " + str(prob_dict[predicate[0]]["pFF"])
        print "    failurep -> " + str(predicate[1]["failurep"])
        print "    context -> " + str(predicate[1]["context"])
        print "    increase -> " + str(predicate[1]["increase"])
        print "    importance -> " + str(predicate[1]["importance"])


main()
