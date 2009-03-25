#!/usr/local/bin/csi -script

(use test extras utils irregex)

(test-begin)

(test-assert (irregex-search "(?u:<..>)" "<漢字>"))
(test-assert (irregex-search "(?u:<.*>)" "<漢字>"))
(test-assert (irregex-search "(?u:<.+>)" "<漢字>"))
(test-assert (not (irregex-search "(?u:<.>)" "<漢字>")))
(test-assert (not (irregex-search "(?u:<...>)" "<漢>")))

(test-assert (irregex-search "(?u:<[^a-z]*>)" "<漢字>"))
(test-assert (not (irregex-search "(?u:<[^a-z]*>)" "<漢m字>")))
(test-assert (irregex-search "(?u:<[^a-z][^a-z]>)" "<漢字>"))
(test-assert (irregex-search "(?u:<あ*>)" "<あ>"))
(test-assert (irregex-search "(?u:<あ*>)" "<ああ>"))
(test-assert (not (irregex-search "(?u:<あ*>)" "<あxあ>")))

(test-assert (irregex-search "(?u:<[あ-ん]*>)" "<あん>"))
(test-assert (irregex-search "(?u:<[あ-ん]*>)" "<ひらがな>"))
(test-assert (not (irregex-search "(?u:<[あ-ん]*>)" "<ひらgがな>")))

(test-end)


