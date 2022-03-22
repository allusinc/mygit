#분석 프로젝트1(실제)
#변수 설명

# Work_Rate 공격 운동량/ 방어 운동량
# Jersey_Number 선수 등번호
# Contract_Valid_Until 계약 만료 년도
# Release_Clause 해체 조항의 금액 (단위 : 1000 유로)
# Value 선수의 현재 시장 가치( 단위 : 1000 유로)
# Wage 주급 ( 단위 : 1000 유로)

fifa<-read.csv(file.choose())

str(fifa)
#명서등비

#미션1

#1피트 30cm
#1인치 2.5cm

#cm로 변환하여 새로운 변수 Height_cm을 생성하시오.

5*30+7*2.5
fifa$Height_cm

str(fifa)

as.numeric(fifa$Height)

as.numeric("57")

?substr()

substr("hi nice", 1,2)

?regexpr()


fifa$Height[1]

regexpr("'", "5'7"-1)

substr("5'7", 1, 1)

regexpr("'", "5'7")

regexpr("'", "5'7")-1

nchar("hi")

nchar("nice")



fifa$Height_cm<-as.numeric(substring(fifa$Height, 1, regexpr("'", fifa$Height)-1))*30 + 
  as.numeric(substring(fifa$Height, regexpr("'", fifa$Height)+1, nchar(fifa$Height)))*2.5


str(fifa)

head(fifa)

table(fifa$Position)

#미션 
#within 함수를 이용하여 선수의 포지션을 의미하는 Position 변수를 
#재범주화하여 Position_Class라는 변수에 저장한다.

# Forward : LS, ST, RS, LW, LF,CF,RF,RW
# Midfielder : LAM, CAM, RAM, LM, LCM, CM, RCM, RM
# Defender : LWB, LDM, CDM, RDM, RWB, LB, LCB, CB, RCB, RB
# GaolKeeper: GK

?within()


#l a m v s f
#df


str(airquality)


aq <- within(airquality, {     # Notice that multiple vars can be changed
  lOzone <- log(Ozone)
  Month <- factor(month.abb[Month])
  cTemp <- round((Temp - 32) * 5/9, 1) # From Fahrenheit to Celsius
  S.cT <- Solar.R / cTemp  # using the newly created variable
  rm(Day, Temp)
})




character(0)

Position_Class <- character(0)

Position_Class

"LS"  %in%  c("LS", "ST", "RS", "LW", "LF","CF","RF","RW")

fifa$Position

str(fifa)

# Forward : LS, ST, RS, LW, LF,CF,RF,RW
# Midfielder : LAM, CAM, RAM, LM, LCM, CM, RCM, RM
# Defender : LWB, LDM, CDM, RDM, RWB, LB, LCB, CB, RCB, RB
# GoalKeeper: GK

fifa<-within(fifa, {
  Position_Class <- character(0)
  Position_Class[ Position  %in%  c("LS", "ST", "RS", "LW", "LF","CF","RF","RW")  ] = "Forward"
  Position_Class[ Position  %in%  c("LAM", "CAM", "RAM", "LM", "LCM", "CM", "RCM", "RM")  ] = "Midfielder"
  Position_Class[ Position  %in%  c("LWB", "LDM", "CDM", "RDM", "RWB", "LB", "LCB", "CB", "RCB", "RB")  ] = "Defender"
  Position_Class[ Position  %in%  c("GK")  ] = "GoalKeeper"
})


str(fifa$Position_Class)

table(fifa$Position_Class)

barplot(table(fifa$Position_Class))
?pie
pie(table(fifa$Position_Class))

View(fifa) # 명목형과 순서형은 가급적 factor로 형변환 처리해서 하여라!!!!!

?factor()

str(fifa$Position_Class)

View(fifa)

fifa$Position_Class<-factor(fifa$Position_Class, levels = c("Forward", "Midfielder","Defender", "GoalKeeper"), labels=c("Forward", "Midfielder","Defender", "GoalKeeper"))

str(fifa$Position_Class)


#새로 생성한 Position_Class 변수의 각 범주에 따른 Value 변수 평균값의 차이를 비교하는 일원배치 분산분석을 
#수행하고 결과를 해석하시오.(데이터는 등분산성 가정을 만족한다고 가정), 그리고 평균값의 차이가
#통계적으로 유의하다면 사후 검정을 수행하고 설명하시오.

#가설 : 귀무, 대립(포지션마다 몸값(value) 차이가 있다. )

#분산분석( analysis of variance : anova)


# formula 종속 ~ 독립(여러개일 경우 + )

?aov

colnames(fifa)

aov(Value ~ Position_Class , data = fifa)


fifa_result<-aov(Value ~ Position_Class , data = fifa)

summary(fifa_result)


# degree of freedom  : n-1

length(fifa$ID)


16642 - 4 
#관측값의 수 - 집단의 수

#결론 : 네가지 포지션에 따른 선수의 시작가치가 모두 동일하지 않다고 결론
#즉 포지션 별 선수의 시장가치(value)의 평균값들 중에서 적어도 어느 하나의 포지션은 통계적으로 유의한 
#차이가 있는 것을 가진다고 말할 수 있다.



















