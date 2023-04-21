#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Mon Apr 13 13:48:27 2020

@author: vxs914
"""
import os
import xml.etree.ElementTree as ET

def write_batch_query(levels, sandbox, outpath):

    # format scenario name (just appending the elements of levels)
    scenName = '-'.join(l for l in levels.astype(str))
    # parse the basic batch query file
    tree = ET.parse('/storage/work/vxs914/GCAM/ensemble_workflow/queries/xmldb_batchTemplate_xh.xml')
    # get tree root
    root = tree.getroot()
    # set db path
    if levels[0] == 0:
        dbpath = f'/gpfs/group/kzk10/default/private/vxs914/GCAM/output/'
    else:
        dbpath = f'/gpfs/group/wvp5117/default/GCAM/output/'
    cnode = root.find('class')
    for node in cnode.findall('.//'):
        if node.tag == 'scenario':
            node.attrib = {'name': scenName}
        if node.tag == 'xmldbLocation':
            node.text = os.path.join(dbpath, f'scendb{scenName}')
        elif node.tag == 'outFile':
            node.text = os.path.join(outpath, f'query_08232021_{scenName}.csv')
        else:
            pass
    # write xml file
    tree.write(os.path.join(sandbox, f'query_08232021_{scenName}.xml'))
