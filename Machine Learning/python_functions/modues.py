import pandas as pd
import numpy as np
from sklearn.preprocessing import StandardScaler
from imblearn.over_sampling import SMOTE
from imblearn.over_sampling import ADASYN
from sklearn.metrics import accuracy_score
from sklearn.metrics import f1_score
from sklearn.metrics import precision_score
from sklearn.metrics import recall_score
import matplotlib.pyplot as plt
from sklearn import metrics
from datetime import datetime, timedelta
from imblearn.combine import SMOTETomek
from imblearn.combine import SMOTEENN


def StandardizerTrainTest(X_train, X_test, standardizationList, binaryList, freqList, nonstdList):
    """Standardization and prevent data leakage

    Args:
        X_train (pd.dataframe): training data
        X_test (pd.dataframe): testing data
        standardizationList (list): continuous variables needed to be standardized 
        binaryList (list): binary variables  

    Returns:
        pd.DataFrame: train, test 
    """
    std = StandardScaler()
    std.fit(X_train[standardizationList])
    x_train_standardized1 = pd.DataFrame(std.transform(X_train[standardizationList]),columns=standardizationList)
    x_train_using = pd.concat([x_train_standardized1,X_train[binaryList], X_train[freqList], X_train[nonstdList]],axis=1)
    # to prevent data leakage 
    x_test_standardized1 = pd.DataFrame(std.transform(X_test[standardizationList]),columns=standardizationList)
    x_test_using = pd.concat([x_test_standardized1,X_test[binaryList], X_test[freqList], X_test[nonstdList]],axis=1)
    return x_train_using, x_test_using

def resampling(X_train, Y_train): 
    """generate the resampling data

    Args:
        X_train (dataframe): X_train
        Y_train (dataframe): X_train

    Returns:
        dataframe: X_smote, Y_smote, X_ADA, Y_ADA
    """
    smote = SMOTE(random_state=527)
    X_smote, Y_smote = smote.fit_resample(X_train,Y_train)
    adasyn = ADASYN(random_state=527)
    X_ADA , Y_ADA = adasyn.fit_resample(X_train, Y_train)
    
    return X_smote, Y_smote, X_ADA, Y_ADA

def _Flexible_test_prediction(new_threshold, test_Prob):
    """Predict the testing result with different threshold probabilities 

    Args:
        new_threshold (float): new threshold to be classified as 1 
        test_Prob (list like): the probability to be 1 in the testing data 

    Returns:
        list like: new binary prediction result 
    """
    res = np.where(test_Prob >= new_threshold, 1, 0)
    return res

def generate_traintest_data_by_time(train_data, region_by):
    '''
    This function generates the training and testing data for each month, based on the time series data, meanwhile it controls the 
    region level by the region_by parameter. the train_10 and test_10 are the "complete" data for the analysis 
    
    train_data: dict, keys are '2019_9', '2019_10', ..., '2020_11', values are the data for each month
    region_by: str, 'District_covid', 'county_covid', 'subcounty_covid' or None 
    
    return: trainData, testData 
    trainData: dict, keys are 'train_2', 'train_3', ..., 'train_10', values are the training data for each month
    testData: dict, keys are 'test_2', 'test_3', ..., 'test_10', values are the testing data for each month
    '''
    train_dateList = [i for i in train_data.keys()]
    train_dataList = [i for i in train_data.values()]

    # Generate the "time series" train and test data
    trainDataStart = train_dataList[0].copy()

    for i in range(1, 3):
        interTrainData = train_dataList[i].copy()
        trainDataStart = pd.concat([trainDataStart, interTrainData], axis=0, ignore_index=True)
    # the first 3 months data is used as the training data (2)
    trainData = {"train_2": trainDataStart.reset_index(drop=True)}
    testData = {"test_2": train_dataList[3].reset_index(drop=True)}

    for i in range(3, len(train_data.keys()) - 1):
        interTrainData = pd.concat([trainData[f"train_{i - 1}"], train_dataList[i].copy()], axis=0, ignore_index=True)
        interTestData = train_dataList[i + 1].copy()
        trainData[f"train_{i}"] = interTrainData.reset_index(drop=True)
        testData[f"test_{i}"] = interTestData.reset_index(drop=True)
        if region_by:
            for key in trainData.keys():
                trainData[key] = trainData[key].query(f"{region_by} != 2 and covid == 0").reset_index(drop=True)
            for key in testData.keys():
                testData[key] = testData[key].query(f"{region_by} == 2").reset_index(drop=True)
    trainData['train_10'] = trainData['train_5']
    # test_10 is the complete during Covid data for the analysis
    testData['test_10'] = pd.concat([testData['test_5'], testData['test_6'], testData['test_7'], testData['test_8'], testData['test_9']], ignore_index=True)
    return trainData, testData

def standardiza_resample_data(train_data, test_data, predictorList, standardizationList, binaryList, freqList, nonstdList):
    '''
    This function generates standardized and resampled data for time series data. Here we standardize the training and 
    testing data, then we only resample the training data. The testing data is not resampled. 
    
    Parameters:
    train_data: dict
        Dictionary of training datasets
    test_data: dict
        Dictionary of testing datasets
    predictorList: list
        List of predictors
    standardizationList: list   
        List of predictors that need to be standardized
    binaryList: list    
        List of binary predictors
    freqList: list  
        List of predictors that need to be frequency encoded
    nonstdList: list    
        List of predictors that need to be non-standardized
        
    Returns:
    StdTrain: dict
        Dictionary of standardized training datasets
    StdTest: dict
        Dictionary of standardized testing datasets
    SMOTE_Train: dict
        Dictionary of SMOTE resampled training datasets
    ADASYN_Train: dict
        Dictionary of ADASYN resampled training datasets
    SMOTETOM_Train: dict
        Dictionary of SMOTETomek resampled training datasets
    SMOTEENN_Train: dict
        Dictionary of SMOTEENN resampled training datasets
    
    ''' 
    # Standardize and resample
    StdTrain = {}
    StdTest = {}
    trainList = [i for i in train_data.values()]
    testList = [i for i in test_data.values()]
    for train, name1 in zip(trainList, train_data.keys()):
        X_train = train[predictorList]
        Y_train = train['FCSStaus']
        std = StandardScaler()
        std.fit(X_train[standardizationList])
        x_train_standardized1 = pd.DataFrame(std.transform(X_train[standardizationList]), columns=standardizationList)
        x_train_using = pd.concat([x_train_standardized1, X_train[binaryList], X_train[freqList], X_train[nonstdList]], axis=1)
        standardizedTrain_withY = pd.concat([x_train_using, Y_train, train['covid'], train['District_covid'], train['county_covid'], train['subcounty_covid']], axis=1)
        StdTrain[f"{name1}"] = standardizedTrain_withY

    for test, name2 in zip(testList, test_data.keys()):
        X_test = test[predictorList]
        Y_test = test['FCSStaus']
        std = StandardScaler()
        std.fit(X_test[standardizationList])
        x_test_standardized1 = pd.DataFrame(std.transform(X_test[standardizationList]), columns=standardizationList)
        x_test_using = pd.concat([x_test_standardized1, X_test[binaryList], X_test[freqList], X_test[nonstdList]], axis=1)
        standardizedTest_withY = pd.concat([x_test_using, Y_test, test['covid'], test['District_covid'], test['county_covid'], test['subcounty_covid']], axis=1)
        StdTest[f"{name2}"] = standardizedTest_withY
    
    smtom = SMOTETomek(random_state=527)
    smenn = SMOTEENN(random_state=527)
    SMOTE_Train = {}
    ADASYN_Train = {}
    SMOTETOM_Train = {}
    SMOTEENN_Train = {}

    for train, name1 in zip(StdTrain.values(), StdTrain.keys()):
        X_smote, Y_smote, X_ADA, Y_ADA = resampling(train[predictorList], train['FCSStaus'])
        X_train_smtom, y_train_smtom = smtom.fit_resample(train[predictorList], train['FCSStaus'])
        X_train_smenn, y_train_smenn = smenn.fit_resample(train[predictorList], train['FCSStaus'])
        SMOTETOM_Train[f"{name1}"] = pd.concat([X_train_smtom, y_train_smtom], axis=1)
        SMOTEENN_Train[f"{name1}"] = pd.concat([X_train_smenn, y_train_smenn], axis=1)
        SMOTE_Train[f"{name1}"] = pd.concat([X_smote, Y_smote], axis=1)
        ADASYN_Train[f"{name1}"] = pd.concat([X_ADA, Y_ADA], axis=1)

    return StdTrain, StdTest, SMOTE_Train, ADASYN_Train, SMOTETOM_Train, SMOTEENN_Train


def standardiza_resample_data_reindex(train_data, test_data, predictorList, standardizationList, binaryList, freqList, nonstdList):
    '''
    This function generates standardized and resampled data for time series data. Here we standardize the training and 
    testing data, then we only resample the training data. The testing data is not resampled. 
    
    Parameters:
    train_data: dict
        Dictionary of training datasets
    test_data: dict
        Dictionary of testing datasets
    predictorList: list
        List of predictors
    standardizationList: list   
        List of predictors that need to be standardized
    binaryList: list    
        List of binary predictors
    freqList: list  
        List of predictors that need to be frequency encoded
    nonstdList: list    
        List of predictors that need to be non-standardized
        
    Returns:
    StdTrain: dict
        Dictionary of standardized training datasets
    StdTest: dict
        Dictionary of standardized testing datasets
    SMOTE_Train: dict
        Dictionary of SMOTE resampled training datasets
    ADASYN_Train: dict
        Dictionary of ADASYN resampled training datasets
    SMOTETOM_Train: dict
        Dictionary of SMOTETomek resampled training datasets
    SMOTEENN_Train: dict
        Dictionary of SMOTEENN resampled training datasets
    
    ''' 
    # Standardize and resample
    StdTrain = {}
    StdTest = {}
    trainList = [i for i in train_data.values()]
    testList = [i for i in test_data.values()]
    for train, name1 in zip(trainList, train_data.keys()):
        X_train = train[predictorList]
        Y_train = train['FCSStaus']
        std = StandardScaler()
        std.fit(X_train[standardizationList])
        x_train_standardized1 = pd.DataFrame(std.transform(X_train[standardizationList]), columns=standardizationList)
        x_train_using = pd.concat([x_train_standardized1.reset_index(), X_train[binaryList].reset_index(), X_train[freqList].reset_index(), X_train[nonstdList].reset_index()], axis=1)
        standardizedTrain_withY = pd.concat([x_train_using.reset_index(), Y_train.reset_index(), train['covid'].reset_index(), train['District_covid'].reset_index(), train['county_covid'].reset_index(), train['subcounty_covid'].reset_index()], axis=1)
        StdTrain[f"{name1}"] = standardizedTrain_withY

    for test, name2 in zip(testList, test_data.keys()):
        X_test = test[predictorList]
        Y_test = test['FCSStaus']
        std = StandardScaler()
        std.fit(X_test[standardizationList])
        x_test_standardized1 = pd.DataFrame(std.transform(X_test[standardizationList]), columns=standardizationList)
        x_test_using = pd.concat([x_test_standardized1.reset_index(), X_test[binaryList].reset_index(), X_test[freqList].reset_index(), X_test[nonstdList].reset_index()], axis=1)
        standardizedTest_withY = pd.concat([x_test_using.reset_index(), Y_test.reset_index(), test['covid'].reset_index(), test['District_covid'].reset_index(), test['county_covid'].reset_index(), test['subcounty_covid'].reset_index()], axis=1)
        StdTest[f"{name2}"] = standardizedTest_withY
    
    smtom = SMOTETomek(random_state=527)
    smenn = SMOTEENN(random_state=527)
    SMOTE_Train = {}
    ADASYN_Train = {}
    SMOTETOM_Train = {}
    SMOTEENN_Train = {}

    for train, name1 in zip(StdTrain.values(), StdTrain.keys()):
        X_smote, Y_smote, X_ADA, Y_ADA = resampling(train[predictorList], train['FCSStaus'])
        X_train_smtom, y_train_smtom = smtom.fit_resample(train[predictorList], train['FCSStaus'])
        X_train_smenn, y_train_smenn = smenn.fit_resample(train[predictorList], train['FCSStaus'])
        SMOTETOM_Train[f"{name1}"] = pd.concat([X_train_smtom, y_train_smtom], axis=1)
        SMOTEENN_Train[f"{name1}"] = pd.concat([X_train_smenn, y_train_smenn], axis=1)
        SMOTE_Train[f"{name1}"] = pd.concat([X_smote, Y_smote], axis=1)
        ADASYN_Train[f"{name1}"] = pd.concat([X_ADA, Y_ADA], axis=1)

    return StdTrain, StdTest, SMOTE_Train, ADASYN_Train, SMOTETOM_Train, SMOTEENN_Train