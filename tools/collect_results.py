import sys
import commands
import os
import re

def space(x) : return x != ''

def process_one_iter(one_file):
    time_to_sol = 0.0
    fits_to_sol = 0.0
    gens_to_sol = 0.0

    converged = False

    compile_time = float(filter(space, commands.getoutput("cat " + one_file + " | grep \"  compile\"").split(" "))[2])
    good_time = float(filter(space, commands.getoutput("cat " + one_file + " | grep \"good test\"").split(" "))[3])
    bad_time = float(filter(space, commands.getoutput("cat " + one_file + " | grep \"bad test\"").split(" "))[3])
    print "FILE: " + one_file
    print compile_time
    print good_time
    print bad_time
    compile_frac = filter(space, commands.getoutput("cat " + one_file + " | grep \"Percent of unique variants that failed to compile\"").split(" "))[8].split("/")
    not_compile = float(compile_frac[0])
    total_compiled = float(compile_frac[1])
    output = commands.getoutput("cat " + one_file + " | grep Best | wc -l")

    if int(output) > 0:
        converged = True
        first_solution_line = commands.getoutput("cat " + one_file + " | grep \"First Solution\"").split(" ")
        time_to_sol = float(first_solution_line[3])
        fits_to_sol = float(first_solution_line[4].split("(")[1])
    else:
        total_time_line = filter(space, commands.getoutput("cat " + one_file + " | grep TOTAL").split(" "))
        time_to_sol = float(total_time_line[1]) # the "total" line doesn't have a count at position 1
        fitness_line = filter(space, commands.getoutput("cat " + one_file + " | grep \"%\" | grep fitness").strip().split(" "))
        fits_to_sol = float(fitness_line[2]) 

        
    gen_solution_line = commands.getoutput("cat " + one_file + " | grep \"Generations to solution:\"")
    gens_to_sol = float(gen_solution_line.split(" ")[3])
#    print "Time to solution: " + str(time_to_sol) + " Fitness evals to solution: " + str(fits_to_sol) + " Generations to solution: " + str(gens_to_sol)
    return (converged, time_to_sol, fits_to_sol, gens_to_sol, compile_time, good_time, bad_time, not_compile, total_compiled)


def main():
    if len(sys.argv) < 2 :
        sys.stdout.write("Usage: " + sys.argv[0] + " file_listing_indirs")

    indirs = open(sys.argv[1], 'r')

    for dir in indirs.readlines():
        split_dir = dir.split(" ")
        print dir
        sys.stdout.write("BENCHMARK: " + dir + "\n")
        sys.stdout.write("file name: " + split_dir[1] + "\n")

        time_to_converge = 0.0
        fitness_to_converge = 0.0
        generations_to_converge = 0.0
        compile_time = 0.0
        good_time = 0.0
        bad_time = 0.0
        not_compile = 0.0
        total_compile = 0.0
        param1_conv_count = 0.0
        param2_conv_count = 0.0
        
        for i in range(0, 100):
            converged = False
            f = split_dir[1].replace('\n', '')
#            param_set1_filename = split_dir[0] + "/" + f + "-" + str(i) + "-0.01-0.06-long.debug"
#            param_set2_filename = split_dir[0] + "/" + f + "-" + str(i) + "-0.00-0.03-long.debug"

            param_set1_filename = split_dir[0] + "/" + f + "-" + str(i) + "-0.01-0.06.debug"
            param_set2_filename = split_dir[0] + "/" + f + "-" + str(i) + "-0.00-0.03.debug"
            
            (converged, time_to_sol1, fits_to_sol1, gens_to_sol1, compile_time1, good_time1, bad_time1, not_compile1, total_compile1) = process_one_iter(param_set1_filename)

            if (converged):
                param1_conv_count += 1.0
                time_to_converge += time_to_sol1
                fitness_to_converge += fits_to_sol1
                generations_to_converge += gens_to_sol1
                compile_time += compile_time1
                good_time += good_time1
                bad_time += bad_time1
                not_compile += not_compile1
                total_compile += total_compile1
            else:
                (converged, time_to_sol2, fits_to_sol2, gens_to_sol2, compile_time2, good_time2, bad_time2, not_compile2, total_compile2) = process_one_iter(param_set2_filename)
                if(converged):
                    generations_to_converge += (gens_to_sol1 + gens_to_sol2)
                    time_to_converge += (time_to_sol1 + time_to_sol2)
                    fitness_to_converge += (fits_to_sol1 + fits_to_sol2)
                    param2_conv_count += 1.0
                    compile_time += (compile_time1 + compile_time2)
                    good_time += (good_time1 + good_time2)
                    bad_time += (bad_time1 + bad_time2)
                    not_compile += not_compile1
                    total_compile += total_compile1


        convergence_rate = (param1_conv_count + param2_conv_count) / (100.0)
        total_converged = param1_conv_count + param2_conv_count
        time_to_converge /= total_converged
        fitness_to_converge /= total_converged
        generations_to_converge /= total_converged
        compile_time /= total_converged
        good_time /= total_converged
        bad_time /= total_converged
        perc_compile = not_compile/total_compile
        print "Total # converged: " + str(total_converged)
        print "Convergence rate: " + str(convergence_rate)
        print "Average time to convergence: " + str(time_to_converge)
        print "Average fitness evaluation to convergence: " + str(fitness_to_converge)
        print "Average number of generations to convergence: " + str(generations_to_converge)
        print "Average time spent compiling: " + str(compile_time)
        print "Average time spent on good tests: " + str(good_time)
        print "Average time spent on bad test: " + str(bad_time)
        print "Percentage not compile: " + str(perc_compile)

main()
