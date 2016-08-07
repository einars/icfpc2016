<?php

echo '<table>';
echo '<tr><td>NPK</td><td>Full size</td><td>Actual size</td></tr>';
for ($i=1; $i <= 101; $i++) {
    printf('<tr><th><a href="%s">%s</a></th><td><img src="%s"></td><td><img src="%s"></td></tr>'
        , sprintf('../all-problems/%05d.txt', $i)
        , sprintf('%05d', $i)
        , sprintf('./images/fit-%03d.png', $i)
        , sprintf('./images/nofit-%03d.png', $i)
    );
}
echo '</table>';
