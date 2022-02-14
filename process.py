# -*- coding: utf-8 -*-
import numpy as np
import pandas as pd
from sympy import symbols, Eq, solve, Symbol


def calculate_dirty_price(clean, last_cp_days):

    dirty_list = []

    for i in range(len(clean)):
        dirty = clean[i] + last_cp_days[i]/365
        dirty = round(dirty, 2)
        dirty_list.append(dirty)

    return dirty_list


def calculate_YTM(cp, dp, yr, fv=100):

    ytm = (fv / dp) ** (1/yr) - 1
    ytm = round(ytm, 4)
    return ytm


if __name__ == "__main__":
    original_pd = pd.read_csv("chosen_data.csv")
    date_titles = ['2022-01-10', '2022-01-11', '2022-01-12', '2022-01-13',
                   '2022-01-14', '2022-01-17', '2022-01-18', '2022-01-19',
                   '2022-01-20', '2022-01-21']
    cp_titles = ['0110_cp_days', '0111_cp_days', '0112_cp_days', '0113_cp_days',
                 '0114_cp_days', '0117_cp_days', '0118_cp_days', '0119_cp_days',
                 '0120_cp_days', '0121_cp_days', ]
    coupons = original_pd['coupon'].values.tolist()
    semi_coupons = original_pd['coupon'].div(2).values.tolist()

    # Calculate the year to maturity of each bond
    # print(original_pd['maturity_date'].values.tolist())
    original_pd["day_diff"] = pd.to_datetime(original_pd['maturity_date']) - \
                      pd.to_datetime("2022-02-01")
    original_pd["day_diff"] = original_pd["day_diff"].apply(lambda x: x.days)
    original_pd["yr_diff"] = original_pd["day_diff"].div(365).round(2)
    yr_diff = original_pd["yr_diff"].values.tolist()


    # -----calculate dirty price----- #
    dirties = []
    cleans = []
    for i in range(10):
        clean = original_pd[date_titles[i]].values.tolist()
        cleans.append(clean)
        cp_days = original_pd[cp_titles[i]].values.tolist()
        dirty = calculate_dirty_price(clean, cp_days)
        dirties.append(dirty)
    # print(dirties)
    # dirties is 10 (date)* 11 (bond dirty price) list
    for i in range(len(dirties)):
        key = date_titles[i] + "_dirty"
        original_pd[key] = pd.DataFrame(dirties[i])
    print(original_pd)
    original_pd.to_csv("final_output.csv")

    bond_dirties = []
    for j in range(11):
        bond = []
        for i in range(10):
            bond.append(dirties[i][j])
        bond_dirties.append(bond)
    bond_cleans = []
    for j in range(11):
        bond = []
        for i in range(10):
            bond.append(cleans[i][j])
        bond_cleans.append(bond)
    for item in bond_cleans:
        print(item)
    print(bond_dirties)
    for item in bond_dirties:
        print(item)

    print(semi_coupons)

    # -----calculate YTM----- #
    # print("Start calculating YTM")
    # for i in range(10):
    #     for j in range(11):
    #         print(calculate_YTM(semi_coupons[j], dirties[i][j], yr_diff[j]))
    # # print(semi_coupons[8], dirties[8][0], yr_diff[8])
    # print("Finish calculating YTM")



