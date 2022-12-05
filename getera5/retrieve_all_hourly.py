#!/usr/bin/env python3
#
# 2016/09/13 Yamashita: first ver. for ERA-interim
# 2019/08/14 Yamashita: for ERA-5
#
# ECMWF info.
# To run this example, you need an API key
# available from https://cds.climate.copernicus.eu/api-how-to
# 2 lines with url and key are written in
# ~/.cdsapirc
#

from datetime import date, timedelta
import time
import os

# time range
nsyy = 1979
nsmm = 1
nsdd = 1
neyy = 2019
nemm = 5
nedd = 31

# data name
data_name = "reanalysis-era5-pressure-levels"  # hourly data on pressure levels from 1979 to present
# data_name = "reanalysis-era5-single-levels" # hourly data on single levels from 1979 to present
# data_name = "reanalysis-era5-land" # Land hourly data from 2001 to present


# output file basename
file_basename_prs = "data_prs"
file_basename_sur = "data_sur"
file_basename_lnd = "data_lnd"

# variables
variables_prs = [
    'divergence', 'fraction_of_cloud_cover', 'geopotential',
    'ozone_mass_mixing_ratio', 'potential_vorticity', 'relative_humidity',
    'specific_cloud_ice_water_content', 'specific_cloud_liquid_water_content',
    'specific_humidity', 'specific_rain_water_content',
    'specific_snow_water_content', 'temperature', 'u_component_of_wind',
    'v_component_of_wind', 'vertical_velocity', 'vorticity'
]
variables_sur = [
    '10m_u_component_of_wind', '10m_v_component_of_wind',
    '2m_dewpoint_temperature', '2m_temperature', 'mean_sea_level_pressure',
    'mean_wave_direction', 'mean_wave_period', 'sea_surface_temperature',
    'significant_height_of_combined_wind_waves_and_swell', 'surface_pressure'
]
variables_lnd = [
    'lake_bottom_temperature', 'lake_ice_depth', 'lake_ice_temperature',
    'lake_mix_layer_depth', 'lake_mix_layer_temperature', 'lake_shape_factor',
    'lake_total_layer_temperature', 'skin_reservoir_content',
    'skin_temperature', 'snow_albedo', 'snow_density',
    'snow_depth_water_equivalent', 'soil_temperature_level_1',
    'soil_temperature_level_2', 'soil_temperature_level_3',
    'soil_temperature_level_4', 'temperature_of_snow_layer',
    'volumetric_soil_water_layer_1', 'volumetric_soil_water_layer_2',
    'volumetric_soil_water_layer_3', 'volumetric_soil_water_layer_4'
]

# pressure levels
pressure_levels = [
    '1', '2', '3', '5', '7', '10', '20', '30', '50', '70', '100', '125', '150',
    '175', '200', '225', '250', '300', '350', '400', '450', '500', '550',
    '600', '650', '700', '750', '775', '800', '825', '850', '875', '900',
    '925', '950', '975', '1000'
]

# hours
hhs = [
    "00", "01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11",
    "12", "13", "14", "15", "16", "17", "18", "19", "20", "21", "22", "23"
]

opt_prs = False
opt_lnd = False
if data_name == "reanalysis-era5-pressure-levels":
    variables = variables_prs
    file_basename = file_basename_prs
    opt_prs = True
elif data_name == "reanalysis-era5-single-levels":
    variables = variables_sur
    file_basename = file_basename_sur
elif data_name == "reanalysis-era5-land":
    variables = variables_lnd
    file_basename = file_basename_lnd
    opt_lnd = True

try:
    import cdsapi
except ImportError:
    raise ImportError("cdsapi must be installed")


def ret_data_prs(yyyy, mm, dd, time_hhmm, file_name):
    c = cdsapi.Client()
    c.retrieve(
        data_name, {
            'format': 'grib',
            'product_type': 'reanalysis',
            'variable': variables,
            'pressure_level': pressure_levels,
            'year': yyyy,
            'month': mm,
            'day': dd,
            'time': time_hhmm,
        }, file_name)


def ret_data_sur(yyyy, mm, dd, time_hhmm, file_name):
    c = cdsapi.Client()
    c.retrieve(
        data_name, {
            'format': 'grib',
            'product_type': 'reanalysis',
            'variable': variables,
            'year': yyyy,
            'month': mm,
            'day': dd,
            'time': time_hhmm,
        }, file_name)


def ret_data_lnd(yyyy, mm, dd, time_hhmm, file_name):
    c = cdsapi.Client()
    c.retrieve(
        data_name, {
            'format': 'grib',
            'variable': variables,
            'year': yyyy,
            'month': mm,
            'day': dd,
            'time': time_hhmm,
        }, file_name)


def main():
    time_start = date(nsyy, nsmm, nsdd)
    time_end = date(neyy, nemm, nedd)
    time_step = timedelta(days=1)

    ofirst = True
    time_now = time_start
    time_bef = time_start - time_step
    while True:
        if time_now <= time_end:
            if (time_now.month != time_bef.month or ofirst):
                fmt = "%Y%m"
                dir_name = str(time_now.strftime(fmt))
                if (os.path.isdir(dir_name)):
                    print("append to " + dir_name)
                else:
                    if (os.path.isfile(dir_name)):
                        os.remove(dir_name)
                    print("mkdir " + dir_name)
                    os.mkdir(dir_name)
            # time
            yyyy = str(time_now.strftime("%Y"))
            mm = str(time_now.strftime("%m"))
            dd = str(time_now.strftime("%d"))
            for hh in hhs:
                time_hhmm = hh + ":00"
                fmt = "%Y%m%d"
                file_name = file_basename + "." + str(
                    time_now.strftime(fmt)) + hh + ".grib"
                file_path = dir_name + '/' + file_name
                print(file_path)
                #
                # data retrieve
                if opt_prs:
                    ret_data_prs(yyyy, mm, dd, time_hhmm, file_path)
                else:
                    if opt_lnd:
                        ret_data_lnd(yyyy, mm, dd, time_hhmm, file_path)
                    else:
                        ret_data_sur(yyyy, mm, dd, time_hhmm, file_path)
                #
                time.sleep(1.0)
            time_bef = time_now
            time_now = time_now + time_step
        else:
            break
        if (ofirst):
            ofirst = False


if __name__ == '__main__':
    main()
