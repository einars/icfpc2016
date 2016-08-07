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
// 23 congruent
// 519 congruent
// 4952 congruent
// 5220 congruent
// 3638 congruent
// 4165 congruent
// 3646 congruent

// define('SOLVER', './cers.bin');
define('SOLVER', 'timeout 30 ./cers.bin');
define('VERSION', 'v23');
define('RMT_ID', '28');

$lock_file = fopen('.lock', 'w+');

function save_problem($pkey)
{
    global $stats, $lock_file;
    do {
        $locked = (flock($lock_file, LOCK_EX | LOCK_NB));
        if ( ! $locked) {
            echo 'lock wait...';
            flush();
            sleep(1);
        }
    } while( ! $locked);
    $f = json_decode(file_get_contents('stats.json'), true);
    $f[$pkey] = $stats[$pkey];
    file_put_contents('stats.json', json_encode($f));
    flock($lock_file, LOCK_UN);
    $stats = $f;

}

function save($x = null)
{
    if ($x !== 'i fuck concurrency') {
        die('DO NOT CALL SAVE');
    }
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
                'md5' => md5_file($spec_file),
                'problem_size' => $p['problem_size'],
                'solution_size' => $p['solution_size'],
            );

            foreach($stats as $k=>$v) {
                if ($v['solved'] && $v['md5'] == $stats[$key]['md5']) {
                    echo ("Warning: the new problem, $key, is already solved as $k.\n");
                }
            }

            $have_new = true;

        }

    }

    if ($have_new) save('i fuck concurrency');

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
    printf('%-5s... ', $spec['id']);
    foreach(array_keys($stats) as $k) {
        $v = $stats[$k];
        if ($v['md5'] == $md5) {
            if ($v['solved'] && $k != $key) {
                // if ($force) {
                    echo("Heresy: $key is already solved as $k!\n");
                // } else {
                    // die("What is this heresy? $key is already solved as $k!");
                // }
            }
            $stats[$k]['tried'] = true;
            $stats[$k]['version'] = VERSION;
            save_problem($k);
        }
    }

    $cmd = "cat {$spec['spec-file']} | " . SOLVER;
    ob_start();
    system($cmd);
    $out = ob_get_clean();

    if ( ! $out || strpos($out, 'SOLUTION: NIL') || strpos($out, 'absent')) {
        $reason = '';
        if (strpos($out, 'SOLUTION: NIL')) $reason = ' (NIL)';
        if (strpos($out, 'absent')) $reason = ' (Candidates)';
        echo "Failed$reason\n";
        foreach(array_keys($stats) as $k) {
            $v = $stats[$k];
            if ($v['md5'] == $md5) {
                $v['tried'] = true;
                $v['version'] = VERSION;
                save_problem($k);
            }
        }
    } else {
        echo "Incredible, solution found!\n";
        foreach(array_keys($stats) as $k) {
            $v = $stats[$k];
            if ($v['md5'] == $md5 and ! $v['solved']) {

                echo "Submitting solution to {$k}\n";
                file_put_contents('/tmp/solution.txt', $out);
                ob_start();
                sleep(4);
                system('curl --silent --compressed -L -H Expect: -H "X-API-Key: 28-b27a5c60566badcfc5d975f3dffdb627" -F "problem_id=' . $v['id'] . '" -F "solution_spec=@/tmp/solution.txt" http://2016sv.icfpcontest.org/api/solution/submit');
                $sub_res = ob_get_clean();

                $res = json_decode($sub_res, true);
                if ($res['ok']) {
                    $stats[$k]['solved'] = true;
                    save_problem($k);
                    show_numbers();
                } else {
                    echo "Submission failed, check it out:\n";
                    echo $sub_res, "\n";
                    if (strpos($sub_res, 'not mapped congruently')) {
                        echo "Ah, that's just the congruence bug. I'll go on\n";
                    } else if (strpos($sub_res, 'not lie on an edge')) {
                        echo "Ah, that's just the edge bug. I'll go on\n";
                    } else {
                        die();
                    }
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
    } while( $stats[$key]['solved'] || ($stats[$key]['tried'] && $stats[$key]['version'] == VERSION));

    return solve($key);
}

function solve_easiest()
{
    global $stats;
    $flt = array();
    foreach($stats as $k=>$v) {
        if ($v['solved']) continue;
        if ($v['tried'] && $v['version'] == VERSION) continue;
        // $flt[$k] = filesize($v['spec-file']);
        $flt[$k] = $v['solution_size'];
    }
    asort($flt);
    $keys = array_keys($flt);
    solve($keys[0]);
}


function solve_sequentially()
{
    global $stats;
    $flt = array();
    foreach($stats as $k=>$v) {
        if ($v['solved']) continue;
        if ($v['tried'] && $v['version'] == VERSION) continue;
        $flt[] = $v['id'];
    }
    sort($flt);
    solve('p' . $flt[0]);
}


if ( ! file_exists('stats.json')) {
    $stats = [];
    save('i fuck concurrency');
} else {
    $stats = json_decode(file_get_contents('stats.json'), true);

}

read_new_problems();

if (isset($argv[1])) {
    solve('p' . $argv[1], true);
} else {
    show_numbers();
    do {
        solve_easiest();
        // solve_sequentially();
        // solve_something_random();
    } while(true);
}
// solve('p50');
