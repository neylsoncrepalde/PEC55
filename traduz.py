#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Artigo sobre a PEC 55
Neylson Crepalde e Maria Alice Silveira
Traduções
Script: Neylson Crepalde
"""
import goslate
import csv
import pandas as pd
import os
from time import sleep

os.chdir('/home/neylson/Documentos/Neylson Crepalde/Doutorado/big_data_projects/PEC55/')

saida = open('dataset_traduzido.csv', 'w')
export = csv.writer(saida, quoting=csv.QUOTE_NONNUMERIC)

gs = goslate.Goslate()
#print(gs.translate('Eu sei o que vcs fizeram no verão passado', 'en'))

dataset_unico = pd.read_csv('dataset_unico.csv', error_bad_lines=False)
#dataset_unico['pt'][:20]

export.writerow(['pt','en'])

for row in range(5):
    pt = dataset_unico['pt'][row]
    en = gs.translate(pt,'en')
    export.writerow([pt,en])
    sleep(5)
    
saida.close()
