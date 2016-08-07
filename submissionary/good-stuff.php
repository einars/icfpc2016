<?php

$f = json_decode(file_get_contents('stats.json'), true);
$seen = array();
foreach($f as $k=>$v) {

    if ($v['tried']) continue;

    if( isset($seen[$v['md5']])) {
        $seen[$v['md5']][] = $k;
    } else {
        $seen[$v['md5']] = [$k];
    }
}

foreach($seen as $k=>$v) {
    if (sizeof($v) > 1) {
        echo implode(', ', $v);
        echo "\n";
    }
}
