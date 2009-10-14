import sys
import commands
import os
import re

crossover_time_to_converge = 0.0
mutation_time_to_converge = 0.0
fitness_time_to_converge = 0.0
bad_test_time_to_converge = 0.0
good_test_time_to_converge = 0.0
compile_time_to_converge = 0.0

temp_crossover_time = 0.0
temp_mutation_time = 0.0
temp_fitness_time = 0.0
temp_bad_test_time = 0.0
temp_good_test_time = 0.0
temp_compile_time = 0.0

num_insertions = 0.0
num_deletions = 0.0
num_swaps = 0.0
num_xover = 0.0
num_mut = 0.0

total_num_insertions = 0.0
total_num_deletions = 0.0
total_num_swaps = 0.0
total_num_xover = 0.0
total_num_mut = 0.0

num_param1_evaled = 0.0
num_param2_evaled = 0.0
time_to_converge = 0.0
total_converged = 0.0
fitness_to_converge = 0.0
temp_total_fitness = 0.0

temp_total_time = 0.0

convergence_rate = 0.0

avg_ins = 0.0
avg_del = 0.0
avg_swap = 0.0
avg_xover = 0.0
avg_mut = 0.0

gens_to_sol = 0.0
time_to_sol = 0.0
fits_to_sol = 0.0
total_sol = 0
global_sol = 0

bad_test_count = 0.0
bad_test_time = 0.0
bad_test_perc = 0.0

mutation_count = 0.0
mutation_time = 0.0
mutation_perc = 0.0

crossover_count = 0.0
crossover_time = 0.0
crossover_perc = 0.0

good_test_count = 0.0
good_test_time = 0.0
good_test_perc = 0.0

fitness_count = 0.0
fitness_time = 0.0
fitness_perc = 0.0

compile_count = 0.0
compile_time = 0.0
compile_perc = 0.0

total_time = 0.0
total_perc = 0.0
total_generations = 0.0
total_fitness = 0.0
total_compile_count = 0.0

failed_to_compile = 0.0
total_compile = 0.0


def average_everything_for_this_benchmark():
    global num_param1_evaled
    global num_param2_evaled
    global avg_ins
    global avg_del
    global avg_swap
    global avg_xover
    global avg_mut
    global total_compile_count
    global failed_to_compile
    global total_compile
    global time_to_converge

    global num_insertions
    global num_deletions
    global num_swaps
    global num_xover
    global num_mut

    global crossover_time_to_converge
    global mutation_time_to_converge
    global fitness_time_to_converge
    global bad_test_time_to_converge
    global good_test_time_to_converge
    global compile_time_to_converge

    average_i_to_converge = num_insertions / total_converged
    average_d_to_converge = num_deletions / total_converged
    average_s_to_converge = num_swaps / total_converged
    average_x_to_converge = num_xover / total_converged
    average_m_to_converge = num_mut / total_converged

    average_fitness_to_converge = fitness_to_converge / total_converged
    average_time_to_convergence = time_to_converge / total_converged
    average_crossover_time_to_converge = crossover_time_to_converge / total_converged
    average_mutation_time_to_converge = mutation_time_to_converge / total_converged
    average_fitness_time_to_converge = fitness_time_to_converge / total_converged
    average_bad_test_time_to_converge = bad_test_time_to_converge / total_converged
    average_good_test_time_to_converge = good_test_time_to_converge / total_converged
    average_compile_time_to_converge = compile_time_to_converge / total_converged

    divisor = num_param1_evaled + num_param2_evaled
    avg_ins = avg_ins / total_fitness
    avg_del = avg_del/ total_fitness
    avg_swap = avg_swap/ total_fitness
    avg_xover = avg_xover/ total_fitness
    avg_mut = avg_mut / total_fitness
    perc_failed_to_compile = failed_to_compile / total_compile
    average_fitness_time = fitness_time / divisor
    average_compile_time = compile_time / divisor
    average_bad_time = bad_test_time / divisor
    average_good_time = good_test_time / divisor
    avg_generations = total_generations / total_sol
    print "convergence rate: " + str(convergence_rate)
    print "avg insertions/fitness: " + str(avg_ins)
    print "avg deletions/fitness: " + str(avg_del)
    print "avg swaps/fitness: " + str(avg_swap)
    print "avg xover/fitness: " + str(avg_xover)
    print "avg mut/fitness: " + str(avg_mut)
    print "% failed to compile: " + str(perc_failed_to_compile)
    print "average time spent on fitness: " + str(average_fitness_time)
    print "average time spent on compile: " + str(average_compile_time)
    print "average time spent on bad tests: " + str(average_bad_time)
    print "average time spent on good tests: " + str(average_good_time)
    print "average generations to convergence: " + str(avg_generations)
    print "average fitness evals to convergence: " + str(average_fitness_to_converge)
    print "average insertions to convergence: " + str(average_i_to_converge)
    print "average deletions to convergence: " + str(average_d_to_converge)
    print "average swaps to convergence: " + str(average_s_to_converge)
    print "average xover to convergence: " + str(average_x_to_converge)
    print "average mutations to convergence: " + str(average_m_to_converge)
    print "average time to convergence: " + str(average_time_to_convergence)

    print "average time spent on crossover to convergence: " + str(average_crossover_time_to_converge)
    print "average time spent on mutation to convergence: " + str(average_mutation_time_to_converge)
    print "average time spent on fitness to convergence: " + str(average_fitness_time_to_converge)
    print "average time spent on bad tests to convergence: " + str(average_bad_test_time_to_converge)
    print "average time spent on good tests to convergence: " + str(average_good_test_time_to_converge)
    print "average time spent on compilation to convergence: " + str(average_compile_time_to_converge)

def process_one_iter(one_file, param_set1):
    global temp_crossover_time
    global temp_mutation_time
    global temp_fitness_time
    global temp_bad_test_time
    global temp_good_test_time
    global temp_compile_time
    global fitness_count
    global avg_ins
    global avg_del
    global avg_swap
    global avg_mut
    global avg_xover
    global total_fitness
    global bad_test_count
    global bad_test_time
    global bad_test_perc
    global good_test_count
    global good_test_time
    global good_test_perc
    global mutation_count
    global mutation_time
    global mutation_perc
    global crossover_count
    global crossover_time
    global crossover_perc
    global fitness_time
    global fitness_perc
    global compile_count
    global compile_time
    global compile_perc
    global total_time
    global total_perc
    global total_sol
    global global_sol
    global time_to_sol
    global fits_to_sol
    global gens_to_sol
    global total_compile_count
    global failed_to_compile
    global total_compile
    global temp_total_time
    global temp_total_fitness
    global num_insertions
    global num_deletions
    global num_swaps
    global num_xover
    global num_mut
    global total_num_insertions
    global total_num_deletions
    global total_num_swaps
    global total_num_xover
    global total_num_mut

    converged = False
    s1_file = open(one_file)
    done = False
    count_lines = 0
    while(not done):
        line = s1_file.readline()
        count_lines += 1
        if(line[0:4] == "Best"):
            converged = True
            done = True
        if(line[0:2] == "No"):
            done = True

    if(converged):
        total_sol = total_sol + 1
        global_sol += 1
        line = s1_file.readline().split(" ")
        time_to_sol = time_to_sol + float(line[3])
        fits_to_sol = fits_to_sol + float(line[4].split("(")[1])
        s1_file.readline() # eat the "best solution" line
        gens_to_sol = float(s1_file.readline().split(" ")[3])
        new_file = open(one_file, 'r').readlines()
        total_for_best = new_file[count_lines-6].split(' ')
        num_insertions += float(total_for_best[0].split('=')[1])
        num_deletions += float(total_for_best[1].split('=')[1])
        num_swaps += float(total_for_best[2].split('=')[1])
        num_xover += float(total_for_best[3].split('=')[1])
        num_mut += float(total_for_best[4].split('=')[1])
        total_num_insertions += float(total_for_best[0].split('=')[1])
        total_num_deletions += float(total_for_best[1].split('=')[1])
        total_num_swaps += float(total_for_best[2].split('=')[1])
        total_num_xover += float(total_for_best[3].split('=')[1])
        total_num_mut += float(total_for_best[4].split('=')[1])

    else:
        s1_file.readline() # eat the "gens to sol" line
    avg_ins_line = s1_file.readline()
    avg_del_line = s1_file.readline()
    avg_swap_line = s1_file.readline()
    avg_xover_line = s1_file.readline()
    avg_mut_line = s1_file.readline()
    temp_total_fitness = float(avg_ins_line.split(" ")[2].split("/")[1])
    avg_ins = avg_ins + (float(avg_ins_line.split("= ")[1]) * temp_total_fitness)
    avg_del = avg_del + (float(avg_del_line.split("= ")[1]) * temp_total_fitness)
    avg_swap = avg_swap + (float(avg_swap_line.split("= ")[1]) * temp_total_fitness)
    avg_xover = avg_xover + (float(avg_xover_line.split("= ")[1])* temp_total_fitness)
    avg_mut = avg_mut + (float(avg_mut_line.split("= ")[1])* temp_total_fitness)
    total_fitness += temp_total_fitness

    line = s1_file.readline()
    failed_to_compile += float(line.split(" ")[4].split("/")[1])
    total_compile += float(line.split(" ")[4].split("/")[0])
    line = s1_file.readline() # eat the header of the table at the bottom
    done = False
    while (not done):
        line = s1_file.readline().split(" ")
        index = line[2]
        i = 0
        if(index == "crossover"):
            i = 5
            while(line[i] == ''):
                i = i + 1
            crossover_count += float(line[i])
            i = i + 1
            while(line[i] == ''):
                i = i + 1
            temp_crossover_time = float(line[i])
            crossover_time += float(line[i])
            crossover_perc += float(line[i+2].replace('%', ''))
        elif(index == "mutation"):
            i = 5
            while(line[i] == ''):
                i = i + 1
            mutation_count += float(line[i])
            i = i + 1
            while(line[i] == ''):
                i = i + 1
            temp_mutation_time = float(line[i])
            mutation_time += float(line[i])
            mutation_perc += float(line[i+2].replace('%', ''))
        elif(index == "fitness"):

            i = 5
            while(line[i] == ''):
                i = i + 1
            fitness_count += float(line[i])
            i = i + 1
            while(line[i] == ''):
                i = i + 1
            temp_fitness_time = float(line[i])
            fitness_time += float(line[i])
            fitness_perc += float(line[i+2].replace('%', ''))
        elif(index == "bad"):
            i = 5
            while(line[i] == ''):
                i = i + 1
            bad_test_count += float(line[i])
            i = i + 1
            while(line[i] == ''):
                i = i + 1
            bad_test_time += float(line[i])
            temp_bad_test_time = float(line[i])
            bad_test_perc += float(line[i+2].replace('%', ''))
        elif(index == "good"):
            i = 5
            while(line[i] == ''):
                i = i + 1
            good_test_count += float(line[i])
            i = i + 1
            while(line[i] == ''):
                i = i + 1
            temp_good_test_time = float(line[i])
            good_test_time += float(line[i])
            good_test_perc += float(line[i+2].replace('%', ''))
        elif(index == "compile"):
            i = 5
            while(line[i] == ''):
                i = i + 1
            compile_count += float(line[i])
            total_compile_count += compile_count
            i = i + 1
            while(line[i] == ''):
                i = i + 1
            temp_compile_time = float(line[i])
            compile_time += float(line[i])
            compile_perc += float(line[i + 2].replace('%', ''))
        elif(index == "TOTAL"):
            done = True
            i = 5
            while(line[i] == ''):
                i = i + 1
            temp_total_time = float(line[i])
            total_time += total_time + float(line[i])
            total_perc += total_perc + float(line[i+2].replace('%', ''))
    return converged


def main():

    global crossover_time_to_converge
    global mutation_time_to_converge
    global fitness_time_to_converge
    global bad_test_time_to_converge
    global good_test_time_to_converge
    global compile_time_to_converge

    global temp_crossover_time
    global temp_mutation_time
    global temp_fitness_time
    global temp_bad_test_time
    global temp_good_test_time
    global temp_compile_time

    global convergence_rate 
    global total_generations
    global num_param1_evaled
    global num_param2_evaled
    global temp_total_time
    global total_converged
    global time_to_converge
    global fitness_count
    global avg_ins
    global avg_del
    global avg_swap
    global avg_mut
    global avg_xover
    global total_fitness
    global bad_test_count
    global bad_test_time
    global bad_test_perc
    global good_test_count
    global good_test_time
    global good_test_perc
    global mutation_count
    global mutation_time
    global mutation_perc
    global crossover_count
    global crossover_time
    global crossover_perc
    global fitness_time
    global fitness_perc
    global compile_count
    global compile_time
    global compile_perc
    global total_time
    global total_perc
    global total_sol
    global time_to_sol
    global fits_to_sol
    global gens_to_sol
    global total_compile_count
    global failed_to_compile
    global fitness_to_converge
    global temp_total_fitness
    global num_insertions
    global num_deletions
    global num_swaps
    global num_xover
    global num_mut

    global total_num_insertions
    global total_num_deletions
    global total_num_swaps
    global total_num_xover
    global total_num_mut
    global global_sol

    if len(sys.argv) < 2 :
        sys.stdout.write("Usage: " + sys.argv[0] + " file_listing_indirs")

    indirs = open(sys.argv[1], 'r')

    for dir in indirs.readlines():
        num_insertions = 0.0
        num_deletions = 0.0
        num_swaps = 0.0
        num_xover = 0.0
        num_mut = 0.0

        temp_crossover_time = 0.0
        temp_mutation_time = 0.0
        temp_fitness_time = 0.0
        temp_bad_test_time = 0.0
        temp_good_test_time = 0.0
        temp_compile_time = 0.0
        crossover_time_to_converge = 0.0
        mutation_time_to_converge = 0.0
        fitness_time_to_converge = 0.0
        bad_test_time_to_converge = 0.0
        good_test_time_to_converge = 0.0
        compile_time_to_converge = 0.0


        param1_conv_count = 0.0
        param2_conv_count = 0.0
        num_param1_evaled = 0.0
        num_param2_evaled = 0.0
        time_to_converge = 0.0
        total_converged = 0.0
        
        temp_total_time = 0.0
        
        convergence_rate = 0.0
        
        avg_ins = 0.0
        avg_del = 0.0
        avg_swap = 0.0
        avg_xover = 0.0
        avg_mut = 0.0
        
        gens_to_sol = 0.0
        time_to_sol = 0.0
        fits_to_sol = 0.0
        total_sol = 0
        
        bad_test_count = 0.0
        bad_test_time = 0.0
        bad_test_perc = 0.0
        
        mutation_count = 0.0
        mutation_time = 0.0
        mutation_perc = 0.0
        
        crossover_count = 0.0
        crossover_time = 0.0
        crossover_perc = 0.0
        
        good_test_count = 0.0
        good_test_time = 0.0
        good_test_perc = 0.0
        
        fitness_count = 0.0
        fitness_time = 0.0
        fitness_perc = 0.0
        
        compile_count = 0.0
        compile_time = 0.0
        compile_perc = 0.0
        real_fitness_temp = 0.0

        total_time = 0.0
        total_perc = 0.0
        total_generations = 0.0
        total_fitness = 0.0
        fitness_to_converge = 0.0
        temp_total_fitness = 0.0
        total_compile_count = 0.0

        failed_to_compile = 0.0
        total_compile = 0.0

        split_dir = dir.split(" ")
        i = 0
        print dir
        sys.stdout.write("BENCHMARK: " + dir + "\n")
        sys.stdout.write("file name: " + split_dir[1] + "\n")

        while i < 100:
            converged = False
            f = split_dir[1].replace('\n', '')
            param_set1_filename = split_dir[0] + "/" + f + "-" + str(i) + "-0.01-0.06.debug"
            param_set2_filename = split_dir[0] + "/" + f + "-" + str(i) + "-0.00-0.03.debug"
            converged = process_one_iter(param_set1_filename, True)
            num_param1_evaled = num_param1_evaled + 1.0
            real_temp = temp_total_time
            real_fitness_temp = temp_total_fitness
            real_crossover_temp = temp_crossover_time 
            real_mutation_temp = temp_mutation_time
            real_fittime_temp = temp_fitness_time
            real_bad_test_temp = temp_bad_test_time
            real_good_test_temp = temp_good_test_time
            real_compile_temp = temp_compile_time

            if(converged): 
                time_to_converge += temp_total_time
                fitness_to_converge += temp_total_fitness
                total_generations += gens_to_sol

                crossover_time_to_converge += temp_crossover_time
                mutation_time_to_converge += temp_mutation_time
                fitness_time_to_converge += temp_fitness_time
                bad_test_time_to_converge += temp_bad_test_time
                good_test_time_to_converge += temp_good_test_time
                compile_time_to_converge += temp_compile_time
                param1_conv_count += 1.0
            else:
                converged = process_one_iter(param_set2_filename, False)
                num_param2_evaled = num_param2_evaled + 1.0
                if(converged):
                    total_generations += 10
                    time_to_converge += temp_total_time + real_temp
                    fitness_to_converge += temp_total_fitness + real_fitness_temp
                    crossover_time_to_converge += temp_crossover_time + real_crossover_temp
                    mutation_time_to_converge += temp_mutation_time + real_mutation_temp
                    fitness_time_to_converge += temp_fitness_time + real_fittime_temp
                    bad_test_time_to_converge += temp_bad_test_time + real_bad_test_temp
                    good_test_time_to_converge += temp_good_test_time + real_good_test_temp
                    compile_time_to_converge += temp_compile_time + real_compile_temp
                    total_generations += gens_to_sol
                    param2_conv_count += 1.0
            i = i + 1

        convergence_rate = (param1_conv_count + param2_conv_count) / (100.0)
        total_converged = param1_conv_count + param2_conv_count
        average_everything_for_this_benchmark()
    print "\n\nTOTALS:"
    print "average insertions to convergence: " + str(total_num_insertions / float(global_sol))
    print "average deletions to convergence: " + str(total_num_deletions / float(global_sol))
    print "average swaps to convergence: " + str(total_num_swaps / float(global_sol))
    print "average xover to convergence: " + str(total_num_xover / float(global_sol))
    print "average mutations to convergence: " + str(total_num_mut / float(global_sol))

main()
