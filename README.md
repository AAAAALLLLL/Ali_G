# Black-Jaguar
My R Markdown for Practical Machine Learning Project
Ali Ghanbari
March 15, 2018
Let’s load the training and testing data sets in r
library(caret)
## Loading required package: lattice
## Loading required package: ggplot2
training<-read.csv("D:\\pml-training.csv")
testing<-read.csv("D:\\pml-testing.csv")
View(training)
ncol(training)
## [1] 160
setting seeds for not randomizing in results
set.seed(1864)
detecting N/A variables
nearzero<-nearZeroVar(training,saveMetrics = TRUE)
Omitting N/A variables from training set
training<-training[,!nearzero$nzv]
ncol(training)
## [1] 100
applying a function for detecting variables with more than 50 % N/A varaibles
rvar<-sapply(colnames(training), function(x)if(sum(is.na(training[,x]))>0.5*nrow(training)){return(TRUE)}else{return(FALSE)})
removing variables with more than N/A values from training set
training<-training[,!rvar]
View(training)
first 6 columns of training set is not useful for prediction so they can be deleted
training<-training[,-c(1:6)]
ncol(training)
## [1] 53
for preprocesss maybe pca can be useful so considering the corr between variables is necessary to be checked
c<-findCorrelation(cor(training[,-53]),cutoff = 0.85)   
showing the name of the variables with more than 85 % corr
names(training[c])
## [1] "accel_belt_z"     "roll_belt"        "accel_belt_y"    
## [4] "accel_belt_x"     "pitch_belt"       "gyros_dumbbell_x"
## [7] "gyros_dumbbell_z" "gyros_arm_x"
c<-findCorrelation(cor(training[,-53]),cutoff = 0.75)  
showing the name of variables with more than 75% corr
names(training[c])  
##  [1] "accel_belt_z"      "roll_belt"         "accel_belt_y"     
##  [4] "accel_arm_y"       "total_accel_belt"  "accel_dumbbell_z" 
##  [7] "accel_belt_x"      "pitch_belt"        "magnet_dumbbell_x"
## [10] "accel_dumbbell_y"  "magnet_dumbbell_y" "accel_arm_x"      
## [13] "accel_dumbbell_x"  "accel_arm_z"       "magnet_arm_y"     
## [16] "magnet_belt_z"     "accel_forearm_y"   "gyros_forearm_y"  
## [19] "gyros_dumbbell_x"  "gyros_dumbbell_z"  "gyros_arm_x"
because most of the variables have good corr with each other pca can be uselful as preprocess. Also repeated cross validation for training set with 5 times repetition applied for reducing overfitting and variance
tc<-trainControl(method = "repeatedcv",number = 5,preProcOptions = "pca") 
the first algorithm to use which is considered is radial svm
svmradial<-train(classe~.,data=training,method="svmRadial",trControl=tc)
confusionMatrix(svmradial)
## Cross-Validated (5 fold, repeated 1 times) Confusion Matrix 
## 
## (entries are percentual average cell counts across resamples)
##  
##           Reference
## Prediction    A    B    C    D    E
##          A 28.2  1.7  0.1  0.1  0.0
##          B  0.1 17.0  0.7  0.0  0.1
##          C  0.1  0.6 16.4  1.7  0.7
##          D  0.0  0.0  0.2 14.6  0.5
##          E  0.0  0.0  0.0  0.0 17.1
##                             
##  Accuracy (average) : 0.9336
then linear svm
svmlinear<-train(classe~.,data=training,method="svmLinear",trControl=tc) 
confusionMatrix(svmlinear)
## Cross-Validated (5 fold, repeated 1 times) Confusion Matrix 
## 
## (entries are percentual average cell counts across resamples)
##  
##           Reference
## Prediction    A    B    C    D    E
##          A 26.1  2.5  1.4  1.1  1.0
##          B  0.6 13.8  1.5  0.6  2.4
##          C  0.8  1.2 13.6  1.8  1.2
##          D  0.8  0.4  0.4 12.1  1.0
##          E  0.1  1.4  0.4  0.8 12.8
##                             
##  Accuracy (average) : 0.7845
installing the package of neural network
library(nnet)  
modeling neural net for training data set
nn<-train(classe~.,data = training,method="nnet",trControl=tc)
## # weights:  63
## initial  value 25510.958467 
## iter  10 value 24210.510755
## iter  20 value 23963.313730
## iter  30 value 23670.823178
## iter  40 value 23536.424145
## iter  50 value 23327.569048
## iter  60 value 23149.822219
## iter  70 value 23108.547352
## iter  80 value 23010.810597
## iter  90 value 22844.574671
## iter 100 value 22708.624289
## final  value 22708.624289 
## stopped after 100 iterations
## # weights:  179
## initial  value 28650.661087 
## iter  10 value 24331.245700
## iter  20 value 23965.070414
## iter  30 value 23189.012146
## iter  40 value 22800.562543
## iter  50 value 22708.168894
## iter  60 value 22677.780085
## iter  70 value 22581.957468
## iter  80 value 22485.059646
## iter  90 value 22461.944147
## iter 100 value 22454.182491
## final  value 22454.182491 
## stopped after 100 iterations
## # weights:  295
## initial  value 28893.565812 
## iter  10 value 23742.262862
## iter  20 value 23460.456999
## iter  30 value 23184.105864
## iter  40 value 22871.813694
## iter  50 value 22720.236201
## iter  60 value 22458.663805
## iter  70 value 22185.368538
## iter  80 value 21704.903513
## iter  90 value 21465.404341
## iter 100 value 21234.816272
## final  value 21234.816272 
## stopped after 100 iterations
## # weights:  63
## initial  value 26181.263080 
## iter  10 value 24480.459788
## iter  20 value 23999.484666
## iter  30 value 23812.271557
## iter  40 value 23777.474006
## iter  50 value 23760.665900
## iter  60 value 23671.488736
## iter  70 value 23616.979016
## iter  80 value 23589.579651
## iter  90 value 23575.400710
## iter 100 value 23560.285926
## final  value 23560.285926 
## stopped after 100 iterations
## # weights:  179
## initial  value 28274.454025 
## iter  10 value 24535.752646
## iter  20 value 24256.827369
## iter  30 value 24001.055974
## iter  40 value 22886.623877
## iter  50 value 22441.713212
## iter  60 value 22237.258848
## iter  70 value 22124.230910
## iter  80 value 21968.065654
## iter  90 value 21850.920006
## iter 100 value 21738.997409
## final  value 21738.997409 
## stopped after 100 iterations
## # weights:  295
## initial  value 31701.690985 
## iter  10 value 24852.702220
## iter  20 value 24399.962630
## iter  30 value 23938.327607
## iter  40 value 23796.776310
## iter  50 value 23551.124793
## iter  60 value 23388.615050
## iter  70 value 23293.642674
## iter  80 value 23056.164485
## iter  90 value 23024.036563
## iter 100 value 23008.753914
## final  value 23008.753914 
## stopped after 100 iterations
## # weights:  63
## initial  value 25272.435246 
## iter  10 value 24748.720637
## iter  20 value 24657.729868
## iter  30 value 24570.689802
## iter  40 value 24554.667047
## iter  50 value 24486.658332
## iter  60 value 24429.710893
## iter  70 value 24334.642133
## iter  80 value 23641.009376
## iter  90 value 23260.796692
## iter 100 value 23059.202109
## final  value 23059.202109 
## stopped after 100 iterations
## # weights:  179
## initial  value 25631.867011 
## iter  10 value 24070.879334
## iter  20 value 23617.033769
## iter  30 value 23508.922407
## iter  40 value 23381.493922
## iter  50 value 22580.956360
## iter  60 value 22384.985593
## iter  70 value 22336.077307
## iter  80 value 22287.968726
## iter  90 value 22189.086502
## iter 100 value 21544.937436
## final  value 21544.937436 
## stopped after 100 iterations
## # weights:  295
## initial  value 28737.757180 
## iter  10 value 23610.596943
## iter  20 value 23203.192919
## iter  30 value 22796.367941
## iter  40 value 22411.603153
## iter  50 value 22220.143955
## iter  60 value 21990.734556
## iter  70 value 21758.444627
## iter  80 value 21667.826275
## iter  90 value 21530.107872
## iter 100 value 21225.994273
## final  value 21225.994273 
## stopped after 100 iterations
## # weights:  63
## initial  value 26909.444487 
## iter  10 value 26693.931067
## iter  20 value 24705.928013
## iter  30 value 24555.738456
## iter  40 value 24516.983187
## iter  50 value 24423.265681
## iter  60 value 24278.140184
## iter  70 value 24246.412603
## iter  80 value 24191.590112
## iter  90 value 24172.378731
## iter 100 value 24152.694132
## final  value 24152.694132 
## stopped after 100 iterations
## # weights:  179
## initial  value 29283.569968 
## iter  10 value 24752.300390
## iter  20 value 23848.340866
## iter  30 value 23772.103909
## iter  40 value 23547.980766
## iter  50 value 23448.732165
## iter  60 value 23379.703018
## iter  70 value 23237.256453
## iter  80 value 23209.807765
## iter  90 value 23052.443849
## iter 100 value 23011.367306
## final  value 23011.367306 
## stopped after 100 iterations
## # weights:  295
## initial  value 28209.103201 
## iter  10 value 24335.632055
## iter  20 value 24097.333573
## iter  30 value 23949.340237
## iter  40 value 23660.508711
## iter  50 value 23400.938033
## iter  60 value 22192.971154
## iter  70 value 21900.673334
## iter  80 value 21377.302107
## iter  90 value 21121.025293
## iter 100 value 20985.373557
## final  value 20985.373557 
## stopped after 100 iterations
## # weights:  63
## initial  value 26904.355780 
## iter  10 value 24867.098443
## iter  20 value 24575.778487
## iter  30 value 24364.633226
## iter  40 value 24332.384211
## iter  50 value 24186.702158
## iter  60 value 23983.864174
## iter  70 value 23896.754011
## iter  80 value 23844.901502
## iter  90 value 23775.883379
## iter 100 value 23757.224037
## final  value 23757.224037 
## stopped after 100 iterations
## # weights:  179
## initial  value 27248.820644 
## iter  10 value 23074.201599
## iter  20 value 22792.445599
## iter  30 value 22182.358733
## iter  40 value 21889.825756
## iter  50 value 21637.561163
## iter  60 value 21484.838924
## iter  70 value 21239.544105
## iter  80 value 21166.394881
## iter  90 value 20922.456250
## iter 100 value 20880.849746
## final  value 20880.849746 
## stopped after 100 iterations
## # weights:  295
## initial  value 27762.933431 
## iter  10 value 23603.234166
## iter  20 value 22632.521126
## iter  30 value 22399.013603
## iter  40 value 21753.120434
## iter  50 value 21228.154755
## iter  60 value 21018.606908
## iter  70 value 20895.641518
## iter  80 value 20742.848749
## iter  90 value 20656.605838
## iter 100 value 20559.796758
## final  value 20559.796758 
## stopped after 100 iterations
## # weights:  63
## initial  value 25378.366217 
## iter  10 value 24494.654376
## iter  20 value 24187.318978
## iter  30 value 23834.275463
## iter  40 value 23805.261146
## iter  50 value 23617.348410
## iter  60 value 23343.287773
## iter  70 value 23283.821385
## iter  80 value 23129.195468
## iter  90 value 23088.956979
## iter 100 value 23074.184239
## final  value 23074.184239 
## stopped after 100 iterations
## # weights:  179
## initial  value 28110.021909 
## iter  10 value 24566.020994
## iter  20 value 24027.715060
## iter  30 value 23668.122001
## iter  40 value 23490.137347
## iter  50 value 23413.175063
## iter  60 value 23226.194875
## iter  70 value 23160.845217
## iter  80 value 23107.741429
## iter  90 value 22987.437309
## iter 100 value 22862.470398
## final  value 22862.470398 
## stopped after 100 iterations
## # weights:  295
## initial  value 25799.450480 
## iter  10 value 23946.244338
## iter  20 value 23670.033481
## iter  30 value 23307.099231
## iter  40 value 22876.952211
## iter  50 value 22695.600850
## iter  60 value 22589.341126
## iter  70 value 22397.729140
## iter  80 value 21978.820862
## iter  90 value 21869.575352
## iter 100 value 21630.106571
## final  value 21630.106571 
## stopped after 100 iterations
## # weights:  63
## initial  value 26299.718125 
## iter  10 value 24784.157177
## iter  20 value 24611.579335
## iter  30 value 24543.221860
## iter  40 value 24445.283038
## iter  50 value 24223.747942
## iter  60 value 23661.894014
## iter  70 value 23580.801948
## iter  80 value 23523.495887
## iter  90 value 23413.628411
## iter 100 value 23363.345460
## final  value 23363.345460 
## stopped after 100 iterations
## # weights:  179
## initial  value 32219.518246 
## iter  10 value 24579.113061
## iter  20 value 23739.838649
## iter  30 value 23394.180064
## iter  40 value 23001.050943
## iter  50 value 22116.624055
## iter  60 value 21726.582322
## iter  70 value 21680.986451
## iter  80 value 21528.861655
## iter  90 value 21212.309822
## iter 100 value 21057.923564
## final  value 21057.923564 
## stopped after 100 iterations
## # weights:  295
## initial  value 28555.735129 
## iter  10 value 23208.245499
## iter  20 value 22500.983533
## iter  30 value 22301.310865
## iter  40 value 22091.850224
## iter  50 value 21731.721528
## iter  60 value 21510.639405
## iter  70 value 21250.329649
## iter  80 value 21039.832174
## iter  90 value 20955.428866
## iter 100 value 20848.093512
## final  value 20848.093512 
## stopped after 100 iterations
## # weights:  63
## initial  value 25461.395159 
## iter  10 value 23620.290523
## iter  20 value 23202.350069
## iter  30 value 23111.960251
## iter  40 value 22922.872326
## iter  50 value 22776.517107
## iter  60 value 22550.511038
## iter  70 value 22397.141478
## iter  80 value 22320.829989
## iter  90 value 22248.988420
## iter 100 value 22151.293604
## final  value 22151.293604 
## stopped after 100 iterations
## # weights:  179
## initial  value 28562.423410 
## iter  10 value 23911.143700
## iter  20 value 23477.914212
## iter  30 value 22958.233849
## iter  40 value 22260.643866
## iter  50 value 21948.873833
## iter  60 value 21743.395552
## iter  70 value 21582.317611
## iter  80 value 21490.070459
## iter  90 value 21350.500337
## iter 100 value 21324.076070
## final  value 21324.076070 
## stopped after 100 iterations
## # weights:  295
## initial  value 26714.343703 
## iter  10 value 23941.977820
## iter  20 value 23077.106779
## iter  30 value 22839.947257
## iter  40 value 22609.702238
## iter  50 value 22505.650380
## iter  60 value 21842.477131
## iter  70 value 21394.212467
## iter  80 value 21024.214239
## iter  90 value 20802.824613
## iter 100 value 20729.328305
## final  value 20729.328305 
## stopped after 100 iterations
## # weights:  63
## initial  value 27420.730551 
## iter  10 value 26373.015685
## iter  20 value 24973.497909
## iter  30 value 24418.466742
## iter  40 value 24338.049756
## iter  50 value 24316.844639
## iter  60 value 24309.844129
## iter  70 value 24305.078880
## iter  80 value 24248.379313
## iter  90 value 24221.506608
## iter 100 value 24156.869754
## final  value 24156.869754 
## stopped after 100 iterations
## # weights:  179
## initial  value 28122.442581 
## iter  10 value 24867.478847
## iter  20 value 23860.297408
## iter  30 value 23486.756974
## iter  40 value 23014.104606
## iter  50 value 22832.401590
## iter  60 value 22761.389030
## iter  70 value 22519.132519
## iter  80 value 22499.240232
## iter  90 value 22361.554645
## iter 100 value 22343.891313
## final  value 22343.891313 
## stopped after 100 iterations
## # weights:  295
## initial  value 33201.503647 
## iter  10 value 24384.592535
## iter  20 value 23791.172761
## iter  30 value 23048.699840
## iter  40 value 22487.342911
## iter  50 value 21996.246736
## iter  60 value 21621.041338
## iter  70 value 21207.076781
## iter  80 value 21132.965658
## iter  90 value 21031.417768
## iter 100 value 20854.522024
## final  value 20854.522024 
## stopped after 100 iterations
## # weights:  63
## initial  value 25422.087571 
## iter  10 value 24580.817700
## iter  20 value 24392.294766
## iter  30 value 24281.473698
## iter  40 value 24227.255554
## iter  50 value 24126.105514
## iter  60 value 24067.680693
## iter  70 value 24029.368754
## iter  80 value 24020.299489
## iter  90 value 23984.159860
## iter 100 value 23981.026987
## final  value 23981.026987 
## stopped after 100 iterations
## # weights:  179
## initial  value 28421.107973 
## iter  10 value 23928.456891
## iter  20 value 23063.393609
## iter  30 value 22410.518265
## iter  40 value 22278.007136
## iter  50 value 22159.813630
## iter  60 value 21922.031893
## iter  70 value 21695.878886
## iter  80 value 21647.316963
## iter  90 value 21604.832752
## iter 100 value 21586.602518
## final  value 21586.602518 
## stopped after 100 iterations
## # weights:  295
## initial  value 26333.443945 
## iter  10 value 23683.296900
## iter  20 value 23187.785002
## iter  30 value 22625.152075
## iter  40 value 22265.717795
## iter  50 value 22084.845737
## iter  60 value 21906.175814
## iter  70 value 21845.191806
## iter  80 value 21251.401379
## iter  90 value 21161.834419
## iter 100 value 21132.565851
## final  value 21132.565851 
## stopped after 100 iterations
## # weights:  63
## initial  value 28063.891854 
## iter  10 value 24463.306465
## iter  20 value 24112.150503
## iter  30 value 24090.097822
## iter  40 value 24006.882905
## iter  50 value 23751.193461
## iter  60 value 23453.309550
## iter  70 value 23402.166869
## iter  80 value 23398.578295
## iter  90 value 23262.355544
## iter 100 value 23227.872507
## final  value 23227.872507 
## stopped after 100 iterations
## # weights:  179
## initial  value 27828.987630 
## iter  10 value 24255.398443
## iter  20 value 23039.764396
## iter  30 value 22763.501455
## iter  40 value 22445.951499
## iter  50 value 22376.373437
## iter  60 value 22287.568822
## iter  70 value 22222.746308
## iter  80 value 22065.701275
## iter  90 value 21918.948200
## iter 100 value 21870.301841
## final  value 21870.301841 
## stopped after 100 iterations
## # weights:  295
## initial  value 31450.245744 
## iter  10 value 23198.854771
## iter  20 value 22594.884697
## iter  30 value 21991.218781
## iter  40 value 21762.851782
## iter  50 value 21213.658888
## iter  60 value 21164.549095
## iter  70 value 21137.933283
## iter  80 value 21108.417679
## iter  90 value 21000.479143
## iter 100 value 20788.430640
## final  value 20788.430640 
## stopped after 100 iterations
## # weights:  63
## initial  value 25425.968413 
## iter  10 value 24785.670405
## iter  20 value 24726.666389
## iter  30 value 24709.073125
## iter  40 value 24697.067828
## iter  50 value 24646.092177
## iter  60 value 24636.215793
## iter  70 value 24632.386542
## iter  80 value 24630.093564
## iter  90 value 24627.991689
## iter 100 value 24626.458832
## final  value 24626.458832 
## stopped after 100 iterations
## # weights:  179
## initial  value 30189.044109 
## iter  10 value 24729.207369
## iter  20 value 24656.193063
## iter  30 value 24585.870019
## iter  40 value 23815.419995
## iter  50 value 23405.388835
## iter  60 value 23254.297844
## iter  70 value 23156.225866
## iter  80 value 22991.268523
## iter  90 value 22931.291676
## iter 100 value 22845.295191
## final  value 22845.295191 
## stopped after 100 iterations
## # weights:  295
## initial  value 35259.843754 
## iter  10 value 23935.859528
## iter  20 value 23292.022139
## iter  30 value 22926.352901
## iter  40 value 22799.682128
## iter  50 value 22509.830743
## iter  60 value 22466.363169
## iter  70 value 22306.736523
## iter  80 value 22046.604805
## iter  90 value 21941.134228
## iter 100 value 21890.272384
## final  value 21890.272384 
## stopped after 100 iterations
## # weights:  63
## initial  value 27305.009010 
## iter  10 value 26025.627929
## iter  20 value 24098.232232
## iter  30 value 23974.311860
## iter  40 value 23789.648738
## iter  50 value 23785.028741
## iter  60 value 23769.973530
## iter  70 value 23766.545097
## iter  80 value 23760.779320
## iter  90 value 23753.740033
## iter 100 value 23744.881930
## final  value 23744.881930 
## stopped after 100 iterations
## # weights:  179
## initial  value 25994.924970 
## iter  10 value 24799.808990
## iter  20 value 24691.202670
## iter  30 value 24332.650900
## iter  40 value 23965.727440
## iter  50 value 23313.686688
## iter  60 value 23202.537848
## iter  70 value 23148.892962
## iter  80 value 23108.951195
## iter  90 value 23055.405732
## iter 100 value 22895.517250
## final  value 22895.517250 
## stopped after 100 iterations
## # weights:  295
## initial  value 30861.772377 
## iter  10 value 24493.164279
## iter  20 value 23697.185504
## iter  30 value 23440.521683
## iter  40 value 22945.843176
## iter  50 value 22638.843764
## iter  60 value 22383.554811
## iter  70 value 22237.540233
## iter  80 value 22086.256392
## iter  90 value 21950.687693
## iter 100 value 21875.342382
## final  value 21875.342382 
## stopped after 100 iterations
## # weights:  63
## initial  value 25836.809104 
## iter  10 value 24390.856214
## iter  20 value 24143.971436
## iter  30 value 23260.484005
## iter  40 value 23088.065601
## iter  50 value 22778.348529
## iter  60 value 22700.755248
## iter  70 value 22585.471633
## iter  80 value 22507.857332
## iter  90 value 22478.110170
## iter 100 value 22417.569030
## final  value 22417.569030 
## stopped after 100 iterations
## # weights:  179
## initial  value 26710.883351 
## iter  10 value 24939.433380
## iter  20 value 24729.544155
## iter  30 value 24264.345492
## iter  40 value 24039.282605
## iter  50 value 23982.326368
## iter  60 value 23468.459521
## iter  70 value 23373.338265
## iter  80 value 23162.591002
## iter  90 value 23009.287543
## iter 100 value 22965.351144
## final  value 22965.351144 
## stopped after 100 iterations
## # weights:  295
## initial  value 27360.448824 
## iter  10 value 24304.617952
## iter  20 value 23530.421642
## iter  30 value 22903.876164
## iter  40 value 22005.216140
## iter  50 value 21450.625499
## iter  60 value 21350.976725
## iter  70 value 21197.743740
## iter  80 value 21055.937347
## iter  90 value 21023.931804
## iter 100 value 20923.102660
## final  value 20923.102660 
## stopped after 100 iterations
## # weights:  63
## initial  value 27399.065940 
## iter  10 value 25755.123406
## iter  20 value 24868.203116
## iter  30 value 24103.441226
## iter  40 value 23942.179858
## iter  50 value 23870.484943
## iter  60 value 23731.303716
## iter  70 value 23579.174639
## iter  80 value 23428.267975
## iter  90 value 23422.470400
## iter 100 value 23418.365007
## final  value 23418.365007 
## stopped after 100 iterations
## # weights:  179
## initial  value 27294.915444 
## iter  10 value 24442.786500
## iter  20 value 23826.283019
## iter  30 value 23563.258276
## iter  40 value 23486.765783
## iter  50 value 23189.246049
## iter  60 value 23046.533326
## iter  70 value 22753.947793
## iter  80 value 22463.775760
## iter  90 value 22298.110760
## iter 100 value 22173.351438
## final  value 22173.351438 
## stopped after 100 iterations
## # weights:  295
## initial  value 30701.737791 
## iter  10 value 24435.887536
## iter  20 value 23861.175922
## iter  30 value 23592.770967
## iter  40 value 23333.733876
## iter  50 value 23178.900046
## iter  60 value 23008.823623
## iter  70 value 22710.041963
## iter  80 value 22385.183557
## iter  90 value 22274.531917
## iter 100 value 22157.997950
## final  value 22157.997950 
## stopped after 100 iterations
## # weights:  295
## initial  value 32660.182887 
## iter  10 value 29293.727122
## iter  20 value 28630.696666
## iter  30 value 28306.419874
## iter  40 value 28059.408475
## iter  50 value 27877.117493
## iter  60 value 27844.450301
## iter  70 value 27820.681879
## iter  80 value 27772.119978
## iter  90 value 27749.269951
## iter 100 value 27654.602755
## final  value 27654.602755 
## stopped after 100 iterations
confusionMatrix(nn)
## Cross-Validated (5 fold, repeated 1 times) Confusion Matrix 
## 
## (entries are percentual average cell counts across resamples)
##  
##           Reference
## Prediction    A    B    C    D    E
##          A 16.0  2.2  3.3  1.3  1.9
##          B  2.2  6.3  2.3  3.3  4.7
##          C  5.9  3.7  9.7  4.1  3.8
##          D  3.6  3.4  1.6  5.6  3.9
##          E  0.8  3.8  0.7  2.1  4.2
##                             
##  Accuracy (average) : 0.4175
then classification tree applied
tree<-train(classe~.,data = training,method="rpart",trControl=tc) 
confusionMatrix(tree)
## Cross-Validated (5 fold, repeated 1 times) Confusion Matrix 
## 
## (entries are percentual average cell counts across resamples)
##  
##           Reference
## Prediction    A    B    C    D    E
##          A 25.8  8.1  8.1  7.3  2.7
##          B  0.5  6.6  0.6  2.9  2.5
##          C  1.8  3.6  8.3  4.7  3.8
##          D  0.2  1.1  0.5  1.5  1.1
##          E  0.1  0.0  0.0  0.0  8.3
##                             
##  Accuracy (average) : 0.5045
prediction for radial svm
psvmr<-predict(svmradial,testing)
prediction for linear svm
psvml<-predict(svmlinear,testing)    
prediction for neural net
pnn<-predict(nn,testing)
prediction for classification tree
ptree<-predict(tree,testing) 
the accuracy of svm radial was 93.4%, then svm linear was 78.35, following by classification tree 50.01% and neural net with 43.98%. So, the highest accuracy with radial svm selected as the best model for prediction.
psvmr
##  [1] B A B A A E D B A A B C B A E E A B B B
## Levels: A B C D E
