<?php

$f = json_decode(file_get_contents('../blob/blob.json'), true);

foreach($f['problems'] as $p) {

    $id = $p['problem_id'];
    $f = sprintf('%05d.txt', $id);

    if ( ! file_exists($f)) {

        $link = "http://2016sv.icfpcontest.org/problem/view/$id";
        $cache = '.cache/' . $id;


        if ( ! file_exists($cache)) {
            $cmd = 'curl --silent "' . $link . '" -H "Accept-Encoding: gzip, deflate, sdch" -H "Accept-Language: en-US,en;q=0.8" -H "Upgrade-Insecure-Requests: 1" -H "User-Agent: Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/52.0.2743.116 Safari/537.36" -H "Accept: text/html,application/xhtml+xml,application/xml;q=0.9,image/webp,*/*;q=0.8" -H "Referer: http://2016sv.icfpcontest.org/problem/list?page=49" -H "Cookie: username="!fha2Cj83HT2i02Nln5P5ow==?gAJVCHVzZXJuYW1lcQFVAjI4cQKGcQMu"; xsrf_token="!1vJKx4o/nQjnCqhSNf0I+A==?gAJVCnhzcmZfdG9rZW5xAVUQZjRkNGI3ZWE1Y2VhZTJkN3EChnEDLg=="" -H "Connection: keep-alive" -H "Cache-Control: max-age=0" --compressed > ' . $cache;
            // echo $cmd . "\n";
            echo $cache . "\n";
            system($cmd);
            sleep(2);
        }

        if( ! file_exists($cache)) {
            die("Missing cache $id");
        }

        $html = file_get_contents($cache);

        $matches = array();
        $co = preg_match('~rows="10">([\-\d/,\r\n ]+)</textarea~', $html, $matches);
        if ( ! isset($matches[1])) {
            die("No matches $id");
        }

        file_put_contents($f, $matches[1]);
        echo "got $id\n";
        flush();
    }

}
