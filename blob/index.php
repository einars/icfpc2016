<head>
<title>Our problems</title>
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
</style>
<?php

$team_id = 28;
if (isset($_GET['team_id'])) {
    $team_id = (int)$_GET['team_id'];
}

$b = json_decode(file_get_contents('blob.json'), $assoc = true);

$name = '???';

foreach($b['users'] as $u) {
    if ($u['username'] == $team_id) {
        $name = $u['display_name'];
    }
}
foreach($b['leaderboard'] as $l) {
    if ($l['username'] == $team_id) {
        printf('<p>Leaderboard score: <b>%d</b></p>', $l['score']);
    }
}

echo '<h1>' . htmlspecialchars($name) . '</h1>';

echo '<table>';
echo '<tr>
    <th>ID</th>
    <th>Published</th>
    <th>prob_size</th>
    <th>sol_size</th>
    <th>perfect</th>
    <th>imperfect</th>
    <th>points</th>
</tr>';

$pt_total = 0;

foreach($b['problems'] as $p) {
    if ($p['owner'] == $team_id) {
        echo '<tr>';
        printf('<td><a href="http://2016sv.icfpcontest.org/problem/view/%s">%s</a></td>'
            , $p['problem_id']
            , $p['problem_id']
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

        $points = (5000 - $p['solution_size']) / ($n_perfect + 1);
        printf('<td class="r">%.1f</td>'
            , $points
        );

        $pt_total += $points;

        echo '</tr>';
    }
}

echo '<tr>';
printf('<td class="r" colspan="7"><b>%.1f</b></td>', $pt_total);

echo '</tr>';


echo '</table>';
