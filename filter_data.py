# -*- coding: utf-8 -*-
import numpy as np
import pandas as pd


def filter_by_duration(input_pd):
    input_pd["issue_date"] = pd.to_datetime(input_pd["issue_date"])
    input_pd["maturity_date"] = pd.to_datetime(input_pd["maturity_date"])
    input_pd["duration"] = input_pd.apply(
        lambda column: column.maturity_date - column.issue_date, axis=1)
    input_pd["duration_m"] = input_pd.duration / np.timedelta64(1, "M")
    input_pd["duration_y"] = input_pd.duration / np.timedelta64(1, "Y")
    input_pd = input_pd.drop(input_pd[input_pd.duration_y < 5].index)
    input_pd = input_pd.drop(input_pd[input_pd.duration_y > 20].index)
    # input_pd["semi_coupon"] = input_pd.coupon.div(2).round(2)
    # # print(input_pd["semi_coupon"], input_pd["coupon"])
    # input_pd = input_pd.drop(input_pd[input_pd.semi_coupon < 0.7].index)
    # input_pd = input_pd.drop(input_pd[input_pd.semi_coupon > 1.3].index)
    return input_pd


if __name__ == "__main__":

    all_pd = pd.read_csv("all_data.csv")
    # filter the bonds which has duration longer than 5 years
    filter_pd = filter_by_duration(all_pd)
    print(filter_pd)
    filter_pd.to_csv("filtered_data.csv", encoding='utf-8', index=False)
