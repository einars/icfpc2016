<head>
<title>All problems</title>
<meta charset="utf-8">
</head>
<style>
table {
    border-collapse: collapse;
}
th, td {
    padding: 4px 8px;
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
</style>
<?php

$team_id = 28;

$b = json_decode(file_get_contents('../blob/blob.json'), $assoc = true);

echo '<h1>All problems</h1>';

echo '<table>';
echo '<tr>
    <th>&nbsp;</th>
    <th>ID</th>
    <th>&nbsp;</th>
    <th>&nbsp;</th>
    <th>Published</th>
    <th>prob_size</th>
    <th>sol_size</th>
    <th>perfect</th>
    <th>imperfect</th>
</tr>';

$pt_total = 0;

foreach($b['problems'] as $p) {
    $id = $p['problem_id'];

    if ($p['owner'] == $team_id) {
        echo '<tr class="ours">';
    } else {
        echo '<tr>';
    }
    echo '<td>';

    $t = sprintf('../problems/t%05d.png', $id);

    if (file_exists($t)) {
        printf('<img src="%s">', $t);
    }

    echo '</td>';
    printf('<td>%05d</td>'
        , $id
    );
    printf('<td><a href="http://2016sv.icfpcontest.org/problem/view/%s">%s</a></td>'
        , $p['problem_id']
        , 'contest page'
    );
    printf('<td><a href="%05d.txt">%s</a></td>'
        , $p['problem_id']
        , 'text'
    );
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

    foreach($p['ranking'] as $r) {
        if (sprintf('%.3f', $r['resemblance']) == '1.000') {
            $n_perfect += 1;
        } else {
            $n_inperfect += 1;
        }
    }
    printf('<td class="r">%s</td>',
        $n_perfect
    );
    printf('<td class="r">%s</td>',
        $n_inperfect
    );

    echo '</tr>';
}



echo '</table>';
