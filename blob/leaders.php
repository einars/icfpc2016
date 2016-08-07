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

$board_score = array();
$total_score = array();
$score = array();
$users = array();
foreach($b['users'] as $u) {
    $score[$u['username']] = 0;
    $users[$u['username']] = $u['display_name'];
}
foreach($b['leaderboard'] as $u) {
    $board_score[$u['username']] = $u['score'];
    $total_score[$u['username']] = $u['score'];
}

foreach($b['problems'] as $p) {
    $n_perfect = 0;
    $n_inperfect = 0;

    foreach($p['ranking'] as $r) {
        if (sprintf('%.3f', $r['resemblance']) == '1.000') {
            $n_perfect += 1;
        } else {
            $n_inperfect += 1;
        }
    }
    $points = (5000 - $p['solution_size']) / ($n_perfect + 1);
    if ( ! isset($score[$p['owner']])) {
        $score[$p['owner']] = 0;
        $users[$p['owner']] = 'id-' . $p['owner'];
    }
    if ( ! isset($board_score[$p['owner']])) {
        $board_score[$p['owner']] = 0;
        $total_score[$p['owner']] = 0;
    }
    $score[ $p['owner'] ] += $points;
    $board_score[ $p['owner'] ] -= $points;
}

echo '<table>';


echo '<tr><td><td><td>';
echo '<th><a href="?mode=prob">problem score</a></th>';
echo '<th><a href="?mode=solve">solving score</a></th>';
echo '<th><a href="?mode=total">total score</a></th>';
echo '</tr>';
switch(@$_GET['mode']) {
case 'solve':
    $sort = $board_score;
    break;
case 'total':
    $sort = $total_score;
    break;
case 'prob':
default:
    $sort = $score;
    break;
}
arsort($sort);


$idx = 0;
foreach($sort as $uid => $_) {
    if ($score[$uid]== 0) continue;
    $idx++;
    echo '<tr>';
    printf('<td>%d.</td>', $idx);
    printf('<td>%d</td>', $uid);
    printf('<td>%s</td>', htmlspecialchars($users[$uid]));
    printf('<td class="r">%d</td>', $score[$uid]);
    printf('<td class="r">%d</td>', $board_score[$uid]);
    printf('<td class="r">%d</td>', $total_score[$uid]);
    echo '</tr>';
}
echo '</table>';

