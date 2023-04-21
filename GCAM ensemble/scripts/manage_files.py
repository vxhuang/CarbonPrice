import os
import shutil, errno
import xml.etree.ElementTree as ET
import re

def create_sandbox(rank):
    # check if directory corresponding to this exists and if not creates
    sandbox = f'/gpfs/scratch/vxs914/GCAM-{rank}/'
    gcam_core = '/storage/work/vxs914/GCAM/gcam-core/'
    if not os.path.isdir(sandbox):
        shutil.copytree(gcam_core, sandbox)

    return sandbox

def create_config_file(levels, sandbox):
    # level refer to the factor levels for this run, which are ordered as: NDC (0-1), water (0-3), socioeconomics by SSP (0-5), energy demand by SSP (0-5), AGLU by SSP (0-5), fossil fuel costs by SSP (0-5), low-emissions energy costs by low/med/high (0-2), and CCS by lo/high (0-1).

    # format scenario name (just appending the elements of levels)
    scenName = '-'.join(l for l in levels.astype(str))
    # read in base configuration file
    tree=ET.parse('/storage/work/vxs914/GCAM/gcam-core/exe/configuration_template.xml')
    # get tree root
    root=tree.getroot()
    # change scenario name
    root[2][0].text = scenName
    # set output directory
    if levels[0] == 0:
        outPath='/gpfs/group/kzk10/default/private/vxs914/GCAM/output/scendb'
    else:
        outPath='/gpfs/group/wvp5117/default/GCAM/output/scendb'
    root[0][4].text = outPath

    # find scenario components section
    scenComp=root.find('ScenarioComponents')

    # create lists for configuration component locations
    # climate contributions (NDCs)
    ndc=['', '../input/extra/xml/policy/carbon_tax_ndc_contamb.xml']

    # water resources
    # groundwater (hi or lo)
    gw_water=['../input/extra/xml/water-xml/water_supply_hi_wg.xml', '../input/extra/xml/water-xml/water_supply_lo_wg.xml', '../input/extra/xml/water-xml/water_supply_lo_wg.xml', '../input/extra/xml/water-xml/water_supply_hi_wg.xml']

    # water supply
    supply_water=['../input/extra/xml/water-xml/water_supply/water_cc_hadgem_4p5_exp_wg.xml', '../input/extra/xml/water-xml/water_supply/water_cc_miroc_4p5.xml', '../input/extra/xml/water-xml/water_supply/water_cc_hadgem_4p5_exp_wg.xml', '../input/extra/xml/water-xml/water_supply/water_cc_miroc_4p5.xml']

    # hydropower
    hydro_water=['../input/extra/xml/water-xml/hydropower/hydro_hadgem2-es_4p5.xml', '../input/extra/xml/water-xml/hydropower/hydro_miroc-esm-chem_4p5.xml', '../input/extra/xml/water-xml/hydropower/hydro_hadgem2-es_4p5.xml', '../input/extra/xml/water-xml/hydropower/hydro_miroc-esm-chem_4p5.xml']

    # socioeconomics (by SSP)
    socecon=['../input/gcamdata/xml/socioeconomics_SSP1.xml', '../input/gcamdata/xml/socioeconomics_SSP2.xml', '../input/gcamdata/xml/socioeconomics_SSP3.xml', '../input/gcamdata/xml/socioeconomics_SSP4.xml', '../input/gcamdata/xml/socioeconomics_SSP5.xml']

    # energy demand (by SSP)
    ind_en=['../input/gcamdata/xml/industry_incelas_ssp1.xml', '../input/gcamdata/xml/industry_incelas_ssp2.xml', '../input/gcamdata/xml/industry_incelas_ssp3.xml', '../input/gcamdata/xml/industry_incelas_ssp4.xml', '../input/gcamdata/xml/industry_incelas_ssp5.xml']

    cem_en=['../input/gcamdata/xml/cement_incelas_ssp1.xml',
    '../input/gcamdata/xml/cement_incelas_ssp2.xml',
    '../input/gcamdata/xml/cement_incelas_ssp3.xml',
    '../input/gcamdata/xml/cement_incelas_ssp4.xml',
    '../input/gcamdata/xml/cement_incelas_ssp5.xml']

    trans_en=['../input/extra/xml/transportation/transportation_UCD_SSP1.xml', '../input/extra/xml/transportation/transportation_UCD_SSP2.xml', '../input/extra/xml/transportation/transportation_UCD_SSP3.xml', '../input/extra/xml/transportation/transportation_UCD_SSP3.xml', '../input/extra/xml/transportation/transportation_UCD_SSP5.xml']

    build_en=['../input/gcamdata/xml/building_SSP1.xml', '../input/gcamdata/xml/building_SSP2.xml', '../input/gcamdata/xml/building_SSP3.xml', '../input/gcamdata/xml/building_SSP4.xml', '../input/gcamdata/xml/building_SSP5.xml']

    # AGLU (by SSP)
    food_aglu=['../input/gcamdata/xml/food_SSP1.xml', '../input/gcamdata/xml/food_SSP2.xml', '../input/gcamdata/xml/food_SSP3.xml', '../input/gcamdata/xml/food_SSP4.xml', '../input/gcamdata/xml/food_SSP5.xml']

    mgmt_aglu=['../input/gcamdata/xml/ag_prodchange_ssp1_IRR_MGMT.xml', '../input/gcamdata/xml/ag_prodchange_ssp2_IRR_MGMT.xml', '../input/gcamdata/xml/ag_prodchange_ssp3_IRR_MGMT.xml', '../input/gcamdata/xml/ag_prodchange_ssp4_IRR_MGMT.xml', '../input/gcamdata/xml/ag_prodchange_ssp5_IRR_MGMT.xml']

    # Fossil Fuel Costs
    fossil_cost=['../input/gcamdata/xml/resources_SSP1.xml', '../input/gcamdata/xml/resources_SSP2.xml', '../input/gcamdata/xml/resources_SSP3.xml',  '../input/gcamdata/xml/resources_SSP4.xml', '../input/gcamdata/xml/resources_SSP5.xml']

    # Low-emissions costs
    solar_cost=['../input/gcamdata/xml/solar_low.xml', '', '../input/gcamdata/xml/solar_adv.xml']

    wind_cost=['../input/gcamdata/xml/wind_low.xml', '', '../input/gcamdata/xml/wind_adv.xml']

    # geothermal has four because we need to use both of the bottom two for the high-cost case
    geo_cost=['../input/gcamdata/xml/geo_low.xml', '',  '../input/gcamdata/xml/geo_adv.xml', '../input/gcamdata/xml/geo_tech_adv.xml']

    nuclear_cost=['../input/gcamdata/xml/nuclear_low.xml', '', '../input/gcamdata/xml/nuclear_adv.xml']

    # CCS has three because we need to use the first and third for the low case
    ccs_cost=['../input/gcamdata/xml/ccs_supply_lowest.xml', '../input/gcamdata/xml/ccs_supply_high.xml', '../input/gcamdata/xml/no_offshore_ccs.xml']

    # based on factor levels, append appropriate configuration xmls
    # climate policy (NDC)
    if levels[0] == 1:
        clim=ET.SubElement(scenComp, 'Value')
        clim.attrib={'name':'climate_policy'}
        clim.text=ndc[1]

    # water
    gw_supply=ET.SubElement(scenComp, 'Value')
    gw_supply.attrib={'name':'water_supply'}
    gw_supply.text=gw_water[levels[1]]
    hydro=ET.SubElement(scenComp, 'Value')
    hydro.attrib={'name':'climate_hydro'}
    hydro.text=hydro_water[levels[1]]
    runoff=ET.SubElement(scenComp, 'Value')
    runoff.attrib={'name':'climate_runoff'}
    #expanded vs restricted reservoir storage
    runoff.text=supply_water[levels[1]]

    # socioeconomics
    soc=ET.SubElement(scenComp, 'Value')
    soc.attrib={'name': 'socioeconomics'}
    soc.text=socecon[levels[2]]

    # energy demand
    ind=ET.SubElement(scenComp, 'Value')
    ind.attrib={'name':'ind'}
    ind.text=ind_en[levels[3]]
    cem=ET.SubElement(scenComp, 'Value')
    cem.attrib={'name':'cement'}
    cem.text=cem_en[levels[3]]
    trans=ET.SubElement(scenComp, 'Value')
    trans.attrib={'name':'trn'}
    trans.text=trans_en[levels[3]]
    build=ET.SubElement(scenComp, 'Value')
    build.attrib={'name':'bld'}
    build.text=build_en[levels[3]]

    # aglu
    food=ET.SubElement(scenComp, 'Value')
    food.attrib={'name': 'aglu'}
    food.text=food_aglu[levels[4]]
    mgmt=ET.SubElement(scenComp, 'Value')
    mgmt.attrib={'name': 'aglu'}
    mgmt.text=mgmt_aglu[levels[4]]

    # fossil fuel costs
    fossil=ET.SubElement(scenComp, 'Value')
    fossil.attrib={'name':'resource_extraction'}
    fossil.text=fossil_cost[levels[5]]

    # low-carbon costs
    if solar_cost[levels[6]] != '':
        solar=ET.SubElement(scenComp, 'Value')
        solar.attrib={'name':'solar'}
        solar.text=solar_cost[levels[6]]
    if wind_cost[levels[6]] != '':
        wind=ET.SubElement(scenComp, 'Value')
        wind.attrib={'name':'wind'}
        wind.text=wind_cost[levels[6]]
    if nuclear_cost[levels[6]] != '':
        nuc=ET.SubElement(scenComp, 'Value')
        nuc.attrib={'name':'nuclear'}
        nuc.text=nuclear_cost[levels[6]]
    if geo_cost[levels[6]] != '':
        geo=ET.SubElement(scenComp, 'Value')
        geo.attrib={'name':'geothermal'}
        geo.text=geo_cost[levels[6]]
        if levels[6] == 2:
            geo_tech = ET.SubElement(scenComp, 'Value')
            geo_tech.attrib={'name':'geothermal_tech'}
            geo_tech.text=geo_cost[3]

    # CCS
    ccs=ET.SubElement(scenComp, 'Value')
    ccs.attrib={'name':'CCS'}
    ccs.text=ccs_cost[levels[7]]
    if levels[7] == 1:
        ccs_offshore = ET.SubElement(scenComp, 'Value')
        ccs_offshore.attrib={'name':'ccs_offshore'}
        ccs_offshore.text=ccs_cost[2]

    # write configuration file to sandbox
    try:
        tree.write(os.path.join(sandbox, 'exe', 'configuration.xml'))
    except SystemError as err:
        print(f'System Error for case {levels}')

def check_run_success(level, outpath):
    scenName = '-'.join(l for l in level.astype(str))
    f = os.path.join(outpath, f'output_{scenName}.txt')
    return os.path.isfile(f)

def check_no_errors(level, outpath):
    scenName = '-'.join(l for l in level.astype(str))
    f = os.path.join(outpath, f'output_{scenName}.txt')
    if os.path.isfile(f):
        errs = [re.findall(r'^ERROR:-*\d+', line) for line in open(f, 'r')]
        return not any(errs)
    else:
        return False
