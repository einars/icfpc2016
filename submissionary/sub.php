<?php

global $stats;

// 1131 congruent
// 1952  Vertex (22552135\/59586176, 0) must not lie on an edge.
// 1536 congruent
// 1539 congruent
// 4980 edge
// 3637 edge
// 3352 edge
// 3959 edge

// define('SOLVER', './cers.bin');
define('SOLVER', 'timeout --kill-after=10 15 ./cers.bin');
define('VERSION', 'v17');
define('RMT_ID', '28');

function save()
{
    global $stats;

    file_put_contents('stats.json', json_encode($stats, JSON_UNESCAPED_UNICODE | JSON_PRETTY_PRINT | JSON_UNESCAPED_SLASHES));
}

function read_new_problems()
{
    global $stats;
    $blob = json_decode(file_get_contents('../blob/blob.json'), true);
    $have_new = false;
    foreach($blob['problems'] as $p) {
        $id = (int)$p['problem_id'];
        $key = 'p' . $id;

        $spec_file = sprintf('../all-problems/%05d.txt', $id);
        if ( ! file_exists($spec_file)) {
            continue;
        }

        if ($p['owner'] == RMT_ID) {
            unset($stats[$key]);
            continue;
        }

        if (isset($stats[$key])) {
            // ok
        } else {

            $stats[$key] =  array(
                'id' => $id,
                'tried' => false,
                'solved' => false,
                'version' => null,
                'spec-file' => $spec_file,
                'md5' => md5_file($spec_file)
            );

            foreach($stats as $k=>$v) {
                if ($v['solved'] && $v['md5'] == $stats[$key]['md5']) {
                    die("The new problem, $key, is already solved as $k");
                }
            }

            $have_new = true;

        }

    }

    if ($have_new) save();

}

function show_numbers()
{
    global $stats;
    $n_tried = 0;
    $n_solved = 0;
    foreach($stats as $k=>$v) {
        if ($v['tried']) {
            if ($v['solved']) {
                $n_solved++;
            } else {
                $n_tried++;
            }
        }
    }
    if ( $n_tried == 0) return;

    printf("Solved %d out of %d: %d%% efficiency. Good job!\n", $n_solved, $n_tried, ($n_solved * 100 / ($n_solved + $n_tried)));
}


function solve($key, $force = false)
{
    global $stats;
    $spec = $stats[$key];
    $md5 = $spec['md5'];
    echo "Solving {$spec['id']}\n";
    foreach($stats as $k=>$v) {
        if ($v['md5'] == $md5) {
            if ($v['solved']) {
                die("What is this heresy? $key is already solved as $k!");
            }
            $stats[$k]['tried'] = true;
            $stats[$k]['version'] = VERSION;
        }
    }
    save(); // mark tried, for case of timeout

    $cmd = "cat {$spec['spec-file']} | " . SOLVER;
    ob_start();
    system($cmd);
    $out = ob_get_clean();

    if ( ! $out || strpos($out, 'SOLUTION: NIL') || strpos($out, 'RROR: ')) {
        echo "Solving failed\n";
        foreach($stats as $k=>$v) {
            if ($v['md5'] == $md5) {
                $v['tried'] = true;
                $v['version'] = VERSION;
            }
        }
        save();
    } else {
        echo "Incredible, solution found!\n";
        foreach($stats as $k => $v) {
            if ($v['md5'] == $md5) {

                echo "Submitting solution to {$k}\n";
                file_put_contents('/tmp/solution.txt', $out);
                ob_start();
                sleep(3);
                system('curl --silent --compressed -L -H Expect: -H "X-API-Key: 28-b27a5c60566badcfc5d975f3dffdb627" -F "problem_id=' . $v['id'] . '" -F "solution_spec=@/tmp/solution.txt" http://2016sv.icfpcontest.org/api/solution/submit');
                $sub_res = ob_get_clean();

                $res = json_decode($sub_res, true);
                if ($res['ok']) {
                    $stats[$k]['solved'] = true;
                    save();
                } else {
                    echo "Submission failed, check it out:\n";
                    echo $sub_res;
                    die();
                }
            }
        }
    }
}

function solve_something_random()
{
    global $stats;
    do {
        $key = array_rand($stats);
    } while($stats[$key]['tried'] && $stats[$key]['version'] == VERSION);

    return solve($key);
}

function solve_easiest()
{
    global $stats;
    $flt = array();
    foreach($stats as $k=>$v) {
        if ($v['tried'] && $v['version'] == VERSION) continue;
        $flt[$k] = filesize($v['spec-file']);
    }
    asort($flt);
    $keys = array_keys($flt);
    solve($keys[0]);
}


if ( ! file_exists('stats.json')) {
    $stats = [];
    save();
} else {
    $stats = json_decode(file_get_contents('stats.json'), true);

}

read_new_problems();

if (isset($argv[1])) {
    solve('p' . $argv[1], true);
} else {
    show_numbers();
    do {
        // solve_easiest();
        solve_something_random();
    } while(true);
}
// solve('p50');
