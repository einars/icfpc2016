<head>
<title>All problems</title>
<meta charset="utf-8">
</head>
<style>
.show-thumbs {
    float: right;
    padding: 8px 16px;
    background: #eee;
    border-radius: 4px;
}
.show-thumbs:hover {
    background: #ddd;
}
table {
    border-collapse: collapse;
}
th, td {
    padding: 4px 8px;
}
th {
    text-align: left;
}
.r {
    text-align: right;
}
a {
    color: #339;
}
tr.ours td {
  background-color: #dfd;
}
td b {
  color: #36c;
}
</style>
<?php

$team_id = 28;

$show_thumbs = isset($_GET['thumbs']);

if ($show_thumbs) {
    echo '<a class="show-thumbs" href="?">Fuck thumbs</a>';
}  else {
    echo '<a class="show-thumbs" href="?thumbs">show thumbs</a>';
}


$b = json_decode(file_get_contents('../blob/blob.json'), $assoc = true);

$users = array();
foreach($b['users'] as $u) {
    $users[ $u['username'] ] = htmlspecialchars($u['display_name']);
}

foreach($b['leaderboard'] as $l) {
    if ($l['score'] > 100000) {
        $users[ $l['username'] ] = '☆ ' . $users[$l['username']];
    }
}

printf('<h1>All %d problems</h1>', sizeof($b['problems']));

echo '<table>';
echo '<tr>
    <th>&nbsp;</th>
    <th>ID</th>
    <th>&nbsp;</th>
    <th>&nbsp;</th>
    <th>Published</th>
    <th>prob_size</th>
    <th>sol_size</th>
    <th>perf.</th>
    <th>imperf.</th>
    <th>perf bounty</th>
    <th>imperf. bounty</th>
    <th>auth</th>
</tr>';

$pt_total = 0;

foreach($b['problems'] as $p) {
    $id = $p['problem_id'];
    $id5 = sprintf('%05d', $p['problem_id']);

    if ($p['owner'] == $team_id) {
        echo '<tr class="ours">';
    } else {
        echo '<tr>';
    }
    echo '<td>';

    $t = sprintf('../problems/t%05d.png', $id);

    if ($show_thumbs and file_exists($t)) {
        printf('<img width="50" height="50" src="%s">', $t);
    }

    echo '</td>';
    printf('<td>%05d</td>'
        , $id
    );
    printf('<td><a href="http://2016sv.icfpcontest.org/problem/view/%s">%s</a></td>'
        , $p['problem_id']
        , 'contest page'
    );
    if (file_exists("$id5.txt")) {
        printf('<td><a href="%05d.txt">%s</a></td>'
            , $p['problem_id']
            , 'text'
        );
    } else {
        echo '<td><span style="color:#ccc">text</span></td>';
    }
    printf('<td>%s</td>',
        date('d.m.Y H:i', $p['publish_time'])
    );
    printf('<td class="r">%s</td>',
        $p['problem_size']
    );
    printf('<td class="r">%s</td>',
        $p['solution_size']
    );

    $n_perfect = 0;
    $n_inperfect = 0;

    $imp_share = 0.0;
    foreach($p['ranking'] as $r) {
        if (sprintf('%.3f', $r['resemblance']) == '1.000') {
            $n_perfect += 1;
        } else {
            $n_inperfect += 1;
            $imp_share += $r['resemblance'];
        }
    }
    printf('<td class="r">%s</td>',
        $n_perfect
    );
    printf('<td class="r">%s</td>',
        $n_inperfect
    );

    $bounty_perfect = $p['problem_size'] / ($n_perfect + 1 + 1);
    $bounty_imperfect = $p['problem_size'] / ($imp_share + 0.999999);

    printf('<td class="r">%d</td>'
        , $bounty_perfect
    );
    printf('<td class="r">%d</td>'
        , $bounty_imperfect
    );

    printf('<td>%s</td>',
        isset($users[ $p['owner'] ]) ? $users[ $p['owner'] ] : 'system-' . $p['owner']
    );

    echo '</tr>';
}



echo '</table>';
