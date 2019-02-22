######################################################################################################################################
# Refererence: https://github.com/ashkan25/Python-CSV-Column-Remover                                                                 #
# File   : remove_column_script.py                                                                                                   #
# About  : Before we can aggregate the data we pulled off of the EPA website, we want to remove and unnecessary columns.             #
#          Our intention is to take some weight off for a faster loading shiny dashboard                                             #
######################################################################################################################################

import pandas as pd
import glob
import argparse
import os
import fnmatch

# If you are uncomfortable with the command line, set use_default_args to True and change the values for the flags in
# my_args and run script without arguments.
# Default values with no args: {'r': False, 'e': False, 'l': False, 'w': '*.csv', 'f': None}
use_default_args = False
my_args = {'r': False, 'e': False, 'l': False, 'w': '*.csv', 'f': None}


def create_parser():
    my_parser = argparse.ArgumentParser(description='Remove specified columns from CSV')
    my_parser.add_argument('-f', help='File name of file with columns to remove (File must have one column per line)')
    my_parser.add_argument('-w', default="*.csv", help='Wildcard for which files to use. default: "*.csv"')
    my_parser.add_argument('-e', action='store_true', help='Drop all empty columns')
    my_parser.add_argument('-l', action='store_true', help='Drop last column if it\'s empty and has no column name')
    my_parser.add_argument('-r', action='store_true',
                           help='Recursively search for files starting from current directory')
    return my_parser


def parse_args(args):
    # Parse args '-r' and '-w'
    file_paths = get_file_paths(args['w'], args['r'])
	
    # Parse args '-f', '-e', '-l' for each file
    if args['f'] or args['e'] or args['l']:
        for file_path in file_paths:
            drop_columns(args['f'], args['e'], args['l'], file_path)


def get_file_paths(wildcard, is_recursive):
    # Find all files matching pattern in current directory
    if not is_recursive:
        return glob.glob(wildcard)

    # Find all files matching pattern recursively starting from current directory
    path_list = []
    for root, dirnames, filenames in os.walk('.'):
        for filename in fnmatch.filter(filenames, wildcard):
            path = os.path.join(root, filename)
            path_list.append(path)

    return path_list


def drop_columns(file_name, drop_empty_cols, drop_last_col, csv_file_path):
    print(csv_file_path)
    df = pd.read_csv(csv_file_path)

    if file_name:
        df = drop_specified_columns(file_name, df)

    if drop_empty_cols:
        df = drop_empty_columns(df)
    elif drop_last_col:
        df = drop_last_column(df)

    df.to_csv(csv_file_path, index=False)


def drop_specified_columns(file_name, df):
    fp = open(file_name, 'r')
    columns = fp.readlines()
    columns = map(lambda col: col.strip(), columns)
    fp.close()

    return df.drop(columns, axis=1, errors='ignore')


def drop_empty_columns(df):
    return df.dropna(axis=1, how='all')


def drop_last_column(df):
    last_col = df.columns[-1]
    if 'Unnamed' in last_col and df[last_col].isnull().all():
        return df.drop(last_col, axis=1)

    return df


if __name__ == '__main__':
    parser = create_parser()
    arguments = vars(parser.parse_args())

    if use_default_args:
        arguments = my_args

    parse_args(arguments)
