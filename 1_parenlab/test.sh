#!/bin/bash
echo 'CM.make "sources.cm";
Tester.testBF();
Tester.testDC();'| sml
$sml
