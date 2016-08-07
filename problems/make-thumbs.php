<?php
for($i = 1; $i < 10000; $i++) {

    $id = sprintf('%05d', $i);

    if ( ! file_exists("../all-problems/$id.txt")) {
        continue;
    }

    if (file_exists("t$id.png")) {
        continue;
    }

    echo $id . "\n";
    system("../render-thumb/render-thumb.coffee $id");

}
