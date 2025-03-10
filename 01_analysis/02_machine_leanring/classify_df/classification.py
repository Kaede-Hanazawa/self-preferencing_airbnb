import numpy as np
import pandas as pd
import os
from sklearn.model_selection import train_test_split
from sklearn.tree import DecisionTreeClassifier
from sklearn.ensemble import RandomForestClassifier
from sklearn.neighbors import KNeighborsClassifier
from sklearn.svm import SVC
from sklearn.linear_model import LogisticRegression
from sklearn.naive_bayes import GaussianNB
from sklearn.metrics import mean_squared_error
from sklearn.impute import SimpleImputer
# import lightgbm as lgb

def load_and_prepare_data(file_path):
    # データの読み込み
    df = pd.read_excel(file_path)
    df_airbnb = df[df['is_airbnb'] == True]
    df_hotel = df[df['is_airbnb'] == False]

    # 特徴量とターゲット変数を分離
    X = df_hotel.drop([
        'hotel_rank', 'address', 'num_of_supply', 'area', 'day_of_week', 'check_in',
        'host', 'listing_name_1', 'listing_name_2', 'scraped_date'], axis=1)
    Y = df_hotel['hotel_rank']

    # NaN値を処理するためにSimpleImputerを使用
    imputer = SimpleImputer(strategy='median')
    X_imputed = imputer.fit_transform(X)

    # データを8:2の割合で分割
    X_train, X_test, Y_train, Y_test = train_test_split(X_imputed, Y, test_size=0.2, random_state=42)

    return df_airbnb, imputer, X_train, X_test, Y_train, Y_test

def train_and_evaluate_models(X_train, X_test, Y_train, Y_test):
    # 各アルゴリズムでの訓練と評価を行う関数
    def evaluate_model(model, X_train, X_test, Y_train, Y_test):
        model.fit(X_train, Y_train)
        Y_pred = model.predict(X_test)
        rmse = np.sqrt(mean_squared_error(Y_test, Y_pred))
        return rmse, model

    # モデルのリスト
    models = [
        ("Decision Tree", DecisionTreeClassifier(random_state=42)),
        ("Random Forest", RandomForestClassifier(random_state=42)),
        # ("LightGBM", lgb.LGBMClassifier(random_state=42)),
        ("k-NN", KNeighborsClassifier()),
        # ("SVM", SVC(kernel='rbf', C=1, gamma='scale', random_state=42)),
        ("Logistic Regression", LogisticRegression(max_iter=1000, random_state=42)),
        ("Naive Bayes", GaussianNB())
    ]

    # 各モデルの評価
    results = []
    for name, model in models:
        rmse, trained_model = evaluate_model(model, X_train, X_test, Y_train, Y_test)
        results.append((name, rmse, trained_model))
        print(f"{name} RMSE: {rmse}")
        print(f"Trained Model: {trained_model}\n")

    # 最もRMSEが小さいモデルを選択
    best_model_details = min(results, key=lambda x: x[1])
    best_model, best_rmse, best_trained_model = best_model_details

    print(f"Best Model: {best_model} with RMSE: {best_rmse}")
    print(f"Best Trained Model Details: {best_trained_model}")

    return best_trained_model

def predict_and_save_results(df_airbnb, imputer, best_trained_model, output_file_path):
    # df_airbnbの特徴量を準備
    X_airbnb = df_airbnb.drop([
        'hotel_rank', 'address', 'num_of_supply', 'area', 'day_of_week', 'check_in',
        'host', 'listing_name_1', 'listing_name_2', 'scraped_date'], axis=1)

    # NaN値を処理するためにSimpleImputerを使用（同じインスタンスを使用して一貫性を保つ）
    X_airbnb_imputed = imputer.transform(X_airbnb)

    # 最良のモデルで予測
    df_airbnb['hotel_rank'] = best_trained_model.predict(X_airbnb_imputed)

    # 結果を保存
    df_airbnb.to_excel(output_file_path, index=False)
    print(f"Results saved to {output_file_path}")

if __name__ == "__main__":
    # スクリプトのディレクトリパスを取得
    script_dir = os.path.dirname(os.path.abspath(__file__))

    # データファイルのパスを設定
    data_file_path = os.path.join(script_dir, '../../99_data/加工データ/df_for_classification.xlsx')
    output_file_path = os.path.join(script_dir, '../../99_data/加工データ/df_airbnb_predictions.xlsx')

    # データの読み込みと整形
    df_airbnb, imputer, X_train, X_test, Y_train, Y_test = load_and_prepare_data(data_file_path)

    # モデルの学習と評価
    best_trained_model = train_and_evaluate_models(X_train, X_test, Y_train, Y_test)

    # 結果の予測と保存
    predict_and_save_results(df_airbnb, imputer, best_trained_model, output_file_path)
