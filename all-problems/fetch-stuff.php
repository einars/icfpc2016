<?php

$f = json_decode(file_get_contents('../blob/blob.json'), true);

foreach($f['problems'] as $p) {

    $id = $p['problem_id'];
    $f = sprintf('%05d.txt', $id);

    if ( ! file_exists($f)) {

        echo $f . "\n";
        $cmd = 'curl --compressed -L -H Expect: -H "X-API-Key: 28-b27a5c60566badcfc5d975f3dffdb627" http://2016sv.icfpcontest.org/api/blob/' . $p['problem_spec_hash'] . ' > ' . $f;
        system($cmd);
        sleep(3);

        // echo file_get_contents($f);

    }

}
