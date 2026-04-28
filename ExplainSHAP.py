import numpy as np
import pandas as pd

import warnings
import logging

import shap
from tqdm import tqdm
# shap.disable_progressbar = True

class ExplainSHAP:
    autoencoder = None
    num_anomalies_to_explain = None
    reconstruction_error_percent = None
    shap_values_selection = None
    counter = None


    def __init__(self, autoencoder, num_anomalies_to_explain=100, reconstruction_error_percent=0, shap_values_selection='mean'):
        self.autoencoder = autoencoder
        self.num_anomalies_to_explain = num_anomalies_to_explain
        self.reconstruction_error_percent = reconstruction_error_percent
        self.shap_values_selection = shap_values_selection


    def get_top_anomaly_to_explain(self, x_explain):
        pred = self.autoencoder.predict(x_explain, verbose = 0)
        sq_err = (x_explain - pred) ** 2
        mse = sq_err.mean(axis=1)

        return mse.nlargest(self.num_anomalies_to_explain).index
    

    def get_errors_df_per_record(self, record):
        pred = self.autoencoder.predict(np.array([[record]])[0], verbose = 0)[0]
        sq_err = (record.values - pred) ** 2

        errors_df = (
            pd.DataFrame({
                "col_name": record.index,
                "err": sq_err
            })
            .sort_values("err", ascending=False)
            .reset_index(drop=True)
        )

        total_mse = sq_err.mean()

        return errors_df, total_mse
    

    def get_var_with_highest_reconstruction_error(self, total_squared_error, errors_df):
        # soglia di errore da raggiungere
        threshold = self.reconstruction_error_percent * total_squared_error

        # errore cumulato (feature ordinate per errore decrescente)
        cumulative_error = errors_df["err"].cumsum()

        # selezione dove superiamo la soglia
        above_threshold = cumulative_error >= threshold

        # primo punto in cui la soglia viene superata
        num_of_features = above_threshold.to_numpy().argmax() + 1

        return num_of_features, errors_df.head(num_of_features)
    
    
    def explain_data(self, x_train, x_explain, top_k=3):

        top_records_to_explain = self.get_top_anomaly_to_explain(x_explain)
        background = shap.sample(x_train, 100)
        # background = shap.kmeans(x_train, 100)

        rows = []

        for record_idx in tqdm(top_records_to_explain, desc="Variabili processate"):

            record = x_explain.loc[record_idx]
            record_array = record.values.reshape(1, -1)

            pred = self.autoencoder.predict(record_array, verbose = 0)[0]

            df_err, total_mse = self.get_errors_df_per_record(record)

            num_features, df_top_err = self.get_var_with_highest_reconstruction_error(
                total_mse * df_err.shape[0],
                df_err
            )

            top_features = df_top_err["col_name"].values

            for feature_name in top_features:

                feature_idx = list(record.index).index(feature_name)

                # f_i(X): ricostruzione della singola feature
                def f_i(X):
                    return self.autoencoder.predict(X, verbose = 0)[:, feature_idx]

                explainer = shap.KernelExplainer(f_i, background)

                shap_values = explainer.shap_values(record_array, nsamples=100, silent=True)[0]
                # print(record_array)
                # tqdm.write(str(record_array))

                x_i = record_array[0, feature_idx]
                x_i_pred = pred[feature_idx]

                shap_series = pd.Series(shap_values, index=record.index)

                # FIX 1: elimina auto-spiegazione
                shap_series = shap_series.drop(labels=feature_name, errors="ignore")

                # split contributing / offsetting
                if x_i > x_i_pred:
                    contributing = shap_series[shap_series < 0]
                    offsetting = shap_series[shap_series > 0]
                else:
                    contributing = shap_series[shap_series > 0]
                    offsetting = shap_series[shap_series < 0]

                # ordina per impatto assoluto
                contributing = contributing.reindex(
                    contributing.abs().sort_values(ascending=False).index
                ).head(top_k)

                offsetting = offsetting.reindex(
                    offsetting.abs().sort_values(ascending=False).index
                ).head(top_k)

                # salva risultati
                for feat, val in contributing.items():
                    rows.append({
                        "record_id": record_idx,
                        "target_feature": feature_name,
                        "feature": feat,
                        "shap_value": val,
                        "type": "contributing",
                        "true_value": x_i,
                        "pred_value": x_i_pred
                    })

                for feat, val in offsetting.items():
                    rows.append({
                        "record_id": record_idx,
                        "target_feature": feature_name,
                        "feature": feat,
                        "shap_value": val,
                        "type": "offsetting",
                        "true_value": x_i,
                        "pred_value": x_i_pred
                    })

        return pd.DataFrame(rows)

