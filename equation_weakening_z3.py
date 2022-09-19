

# To install z3:
# pip install z3-solver -i https://mirrors.aliyun.com/pypi/simple

from z3 import *
from subprocess import Popen, PIPE
import sys

def is_number(x):
    try:
        float(x)
        return True
    except:
        return False

def replace_nums_with_vars(sym_list, var_list, num_new_vars):
    if num_new_vars <= 0:
        return [[sym_list, var_list]]
    if num_new_vars > 0 and sym_list == []:
        return []

    results = []
    for i in range(len(sym_list)):
        # Here, we only consider the case that sym_list[i] is the first occurence, i.e., sym_list[i] does not occur in sym_list[0:i].
        s1 = sym_list[0:i]
        if is_number(sym_list[i]) == True and not(sym_list[i] in s1):
            if var_list == []:
                new_var = "k0"
            else:
                new_var = "k" + str(int(var_list[-1][0].replace("k",""))+1)

            #s2 = sym_list[i+1:len(sym_list)]
            # replace all occurences of sym_list[i] with new_var.
            s2 = [(new_var if x == sym_list[i] else x) for x in sym_list[i+1:len(sym_list)]]

            sub_results = replace_nums_with_vars(s2, var_list + [[new_var, sym_list[i]]], num_new_vars-1)
            for sr in sub_results:
                r = [s1 + [new_var] + sr[0], sr[1]]
                results.append(r)
    return results


def equation_weakening(src_eq, max_num_vars):

    # Check if the equation can be true without weakening.
    eq_tmp = src_eq + ""
    eq_solver = Solver()
    flag_int = Real("flag_int")
    eq_tmp = eq_tmp.split("<AND>")
    for eq in eq_tmp:
        eq_solver.add(And(eval(eq), flag_int == 0))
    eq_solver.check()
    try:
        sol = eq_solver.model()
        print("\'The equation is true. It will not be weakened.\'")
        return []
    except:
        print("\'The equation is false. It will be weakened.\'")


    # Weakening the equation... 
    eq = src_eq + ""
    eq = eq.replace("**", " <power> ").replace("e+", "<power10pos>").replace("e-", "<power10neg>")
    eq = eq.replace("+", " + ").replace("-", " - ").replace("*", " * ").replace("/", " / ").replace("(", " ( ").replace(")", " ) ")
    eq = eq.replace("<power10pos>", "e+").replace("<power10neg>", "e-")
    eq = eq.replace("  "," ").split(" ")

    for n in range(1, max_num_vars + 1):
        weakened_eqs = replace_nums_with_vars(sym_list = eq, var_list = [], num_new_vars = n)
        results = []
        has_valid_result = False
        for x in weakened_eqs:
            eq_tmp = "".join(x[0])
            eq_tmp = eq_tmp.replace("<power>", "**")
            for var_id in [v[0] for v in x[1]]:
                solver_cmd = "%s = Real(\'%s\')"%(var_id, var_id)
                exec(solver_cmd)
            # print("##### RESULT #####")
            # print("Equation:\n" + eq_tmp)
            # print("Weakened Parameters:\n" + str(x[1]))
            # print("Solutions:")
            eq_solver = Solver()
            eq_tmp = eq_tmp.split("<AND>")
            for q in eq_tmp:
                eq_solver.add(eval(q))
            #eq_solver.add("And(%s)"%eq_tmp)
            eq_solver.check()
            try:
                sol = eq_solver.model()
                # for r in sol:
                #     print([r, sol[r]])
                if len(sol) == n:
                    has_valid_result = True
                    res = [eq_tmp]
                    for r in sol:
                        for p in x[1]:
                            if str(r) == p[0]:
                                res.append([p[0], p[1], str(sol[r])])
                                break
                else:
                    res = [eq_tmp, "Illegal Solutions", str(sol).replace("\n", ", ")]
                results.append(res)
            except:
                res = [eq_tmp, "No solution found."]

            #input(res)

            # solver_cmd = "solve(%s)"%eq_tmp
            # exec(solver_cmd)
            # input("Pause")
            # print("##### END #####")
        if has_valid_result == True:
            return results


def get_valid_solutions_for_prolog(z3_solutions):
    results = []
    for x in z3_solutions:
        if "Illegal Solutions" in str(x):
            continue
        sol = ["replace(%s, %s)"%(y[1], y[2]) for y in x[1:len(x)]]
        sol = str(sol).replace("\'","").replace("\"","")
        results.append(sol)
    return results

eq = sys.argv[1]

# Test case 1:
"""
eq = "6.7e-11*Q1*Q2/((X2-X1)**2+(Y2-Y1)**2+(Z2-Z1)**2)*(X2-X1)/((X2-X1)**2+(Y2-Y1)**2+(Z2-Z1)**2)**0.5 == 1.44e-3 <AND> 6.7e-11*Q1*Q2/((X2-X1)**2+(Y2-Y1)**2+(Z2-Z1)**2)*(X2-X1)/((X2-X1)**2+(Y2-Y1)**2+(Z2-Z1)**2)**0.5 == 1.44e-3"
eq = eq.replace("Q1","4e-8").replace("Q2","4e-8").replace("X1","0.1").replace("Y1","0").replace("Z1","0").replace("X2","0").replace("Y2","0").replace("Z2","0")
"""

# Test case 2:
"""
eq = "6.7e-11*Q1*Q2/((X2-X1)**2+(Y2-Y1)**2+(Z2-Z1)**2)*(X2-X1)/((X2-X1)**2+(Y2-Y1)**2+(Z2-Z1)**2)**0.5 == 1.44e-3 <AND> 7.2e-10*Q1*Q2/((X2-X1)**2+(Y2-Y1)**2+(Z2-Z1)**2)*(X2-X1)/((X2-X1)**2+(Y2-Y1)**2+(Z2-Z1)**2)**0.5 == 2.44e-3"
eq = eq.replace("Q1","4e-8").replace("Q2","4e-8").replace("X1","0.1").replace("Y1","0").replace("Z1","0").replace("X2","0").replace("Y2","0").replace("Z2","0")
"""

print("[")
res = equation_weakening(src_eq = eq, max_num_vars = 3)
res = get_valid_solutions_for_prolog(res)
for x in res:
    print("," + x)
print("]")
