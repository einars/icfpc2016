<head>
<title>All problems</title>
<meta charset="utf-8">
</head>
<style>
* { box-sizing: border-box }
.head {
    position: fixed;
    left: 0;
    top: 0;
    height: 90px;
    width:100%;
    border-bottom: 1px solid #ccc;;
    background: rgba(255, 255, 255, 0.85);
    padding: 8px;
}

table {
    margin-top: 100px;
}
p.links {
    margin: 0;
    color: #333;
}
.imp {
 display: inline-block; width: 40px; text-align: left; color:#777;
font-size: 80%;
}
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

.img {
    display: inline-block;
    width: 16px;
    height: 16px;
}
.solved .img,
.solved img {
    background-color: #dfd;
}
.tried .img, .tried img {
    background-color: #fdd;
}
</style>
<?php

$team_id = 28;

$show_thumbs = isset($_GET['thumbs']);
$hard_only = isset($_GET['hard']);
$easy_only = isset($_GET['easy']);

$tried_only = isset($_GET['tried']);
$solved_only = isset($_GET['solved']);

$txt_missing = 0;

$solution_status = json_decode(file_get_contents('../submissionary/stats.json'), true);

echo '<div class="head">';

if ($show_thumbs) {
    echo '<a class="show-thumbs" href="?">Fuck thumbs</a>';
}  else {
    echo '<a class="show-thumbs" href="?thumbs">show thumbs</a>';
}

echo '<p class="links">supported url params: <a href="?tried&thumbs">tried</a>, <a href="?solved&thumbs">solved</a>, <a href="?hard">hard</a>, <a href="?easy">easy</a>, <a href="?thumbs">thumbs</a></p>';

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

$n_problems = 0;

ob_start();
echo '<table>';
echo '<tr>
    <th>&nbsp;</th>
    <th>ID</th>
    <th>&nbsp;</th>
    <th>&nbsp;</th>
    <th>size</th>
    <th>solvers.</th>
    <th>bounty</th>
    <th>auth</th>
</tr>';

$pt_total = 0;

foreach($b['problems'] as $p) {
    $id = $p['problem_id'];
    $id5 = sprintf('%05d', $p['problem_id']);

    $n_perfect = 0;
    $n_imperfect = 0;

    $tried = false;
    $solved = false;

    $sol_key = 'p' . $id;
    if (isset($solution_status[$sol_key])) {
        if ($solution_status[$sol_key]['solved']) {
            $solved = true;
            $tried = true;
        } else if ($solution_status[$sol_key]['tried']) {
            $tried = true;
        }
    }


    $imp_share = 0.0;
    foreach($p['ranking'] as $r) {
        if (sprintf('%.3f', $r['resemblance']) == '1.000') {
            $n_perfect += 1;
        } else {
            $n_imperfect += 1;
            $imp_share += $r['resemblance'];
        }
    }

    if ($hard_only and ($n_perfect > 1)) continue;
    if ($easy_only and ($n_perfect < 10)) continue;
    if ($tried_only and ( ! $tried)) continue;
    if ($solved_only and ( ! $solved)) continue;

    $n_problems++;
    if ($p['owner'] == $team_id) {
        echo '<tr class="ours">';
    } else if ($solved) {
        echo '<tr class="solved">';
    } else if ($tried) {
        echo '<tr class="tried">';
    } else {
        echo '<tr>';
    }
    echo '<td>';

    $t = sprintf('../problems/t%05d.png', $id);

    if ($show_thumbs and file_exists($t)) {
        printf('<img width="50" height="50" src="%s">', $t);
    } else {
        echo '<span class="img"></span>';
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
        $txt_missing += 1;
        echo '<td><span style="color:#999">text</span></td>';
    }
    printf('<td class="r">%s <span class="imp">/ %s</span></td>'
        , $p['problem_size']
        , $p['solution_size']
    );

    printf('<td class="r">%s <span class="imp">/ %s</span></td>'
        , $n_perfect
        , $n_imperfect
    );

    $bounty_perfect = $p['solution_size'] / ($n_perfect + 1 + 1);
    $bounty_imperfect = ($p['solution_size'] / ($n_perfect + 1)) / ($imp_share + 0.999999);

    printf('<td class="r"><b>%d</b> <span class="imp">/ %d</span></td>'
        , $bounty_perfect
        , $bounty_imperfect
    );

    printf('<td>%s</td>',
        isset($users[ $p['owner'] ]) ? $users[ $p['owner'] ] : 'system-' . $p['owner']
    );

    echo '</tr>';
}



echo '</table>';

$g = ob_get_clean();
printf('<h3>%d problems</h3>', $n_problems);

echo '</div>'; // .head

echo $g;


if ($txt_missing) {
    echo '<p>';
    printf('%d missing problems, expect them in about %d minutes', $txt_missing, ($txt_missing / 22));
    echo '</p>';
}
