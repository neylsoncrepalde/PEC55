#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Artigo sobre a PEC 55
Neylson Crepalde e Maria Alice Silveira
Traduções
Script: Neylson Crepalde
"""
import mtranslate as mt
import csv
import pandas as pd
import os

os.chdir('D:/Neylson Crepalde/PEC55/')

saida = open('dataset_traduzido2.csv', 'w')
export = csv.writer(saida, quoting=csv.QUOTE_NONNUMERIC)


#print(mt.translate('Eu sei o que vcs fizeram no verão passado','en','pt'))

dataset = pd.read_csv('dataset.csv', error_bad_lines=False)
#dataset_unico['pt'][:20]

export.writerow(['pt','en'])

for row in range(len(dataset.index)):
	try:
	    pt = dataset['pt'][row]
	    en = mt.translate(pt,'en','pt')
	    export.writerow([pt,en])
	
	except UnicodeEncodeError as e:
		continue
	except urllib.error.HTTPError as urlerror:
		continue
saida.close()

print('Acabou.')
