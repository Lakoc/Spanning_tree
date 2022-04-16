import os
from subprocess import Popen, PIPE
import argparse


def parse_trees(file):
    rows = file.split('\n')
    trees = [sorted(row.split(' ')) for row in rows if row != '']
    return sorted(trees)


def same_trees(t1, t2):
    return t1 == t2


if __name__ == '__main__':
    parser = argparse.ArgumentParser(description='Process tests in specified folder and compare to provided output.')
    parser.add_argument('executable', type=str, help="Executable path.")
    parser.add_argument('folder', type=str, help="Folder with tests.")

    args = parser.parse_args()

    tests = list(set([file.split('.')[0] for file in os.listdir(args.folder) if file.endswith('.in')]))

    for test in tests:
        cmd = [f'./{args.executable}']
        p = Popen(cmd, stdin=PIPE, stdout=PIPE, stderr=PIPE,
                  bufsize=-1)
        with open(f'{os.path.join(args.folder, test)}.in', 'rb') as input_f:
            with open(f'{os.path.join(args.folder, test)}.out') as f:
                ref_output = f.read()
                ref = parse_trees(ref_output)
                output, error = p.communicate(input=input_f.read())
                hyp = parse_trees(output.decode("utf-8"))
                if not same_trees(ref, hyp):
                    missing = [item for item in ref if item not in hyp]
                    not_in_ref = [item for item in hyp if item not in ref]
                    raise Exception(f"Test file: <{test}> failed.\n{output.decode('utf-8')}\n"
                                    f"Missing {missing}\n"
                                    f"Not in reference {not_in_ref}")
    print("All tests passed.")
