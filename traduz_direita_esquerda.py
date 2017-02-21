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
import urllib

os.chdir('/home/neylson/Documentos/Neylson Crepalde/Doutorado/Artigos meus/PEC55/')

saida = open('direita_posts_traduzido.csv', 'w')
export = csv.writer(saida, quoting=csv.QUOTE_NONNUMERIC)


#print(mt.translate('Eu sei o que vcs fizeram no verão passado','en','pt'))

dataset = pd.read_csv('direita_posts.csv', error_bad_lines=False)
dataset.columns = ['pt']
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
