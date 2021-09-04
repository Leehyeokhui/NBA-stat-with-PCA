# Regression with PCA
R program을 이용한 분석
NBA 선수들의 시즌 stat data를 이용하여 PCA와 회귀분석을 통해 NBA 선수들의 연봉에 미치는 요소를 확인  

분석 과정  
1. EDA 및 outlier 제거 - outlier에 있는 선수들은 최저연봉으로 단기계약을 하거나 maximum 계약이라는 특수한 형태의 계약이기 때문에 제거하고 진행
2. 회귀모델에 적합
 - 이 과정에서 변수 간 다중공선성이 너무 높게 나오는 문제 발견  
 - 다중공선성 문제 해결을 위해서는 상관관계가 높은 변수를 제거하거나 파생변수 생성, 차원축소 등의 방법을 사용
 - 파생변수 생성을 위해 per 지수 등의 지표를 찾았지만 stat data 특성 상 대부분의 변수가 높은 상관관계를 가졌기 때문에 모든 변수 간 다중공선성을 해결하기 힘듦
 - 따라서 PCA를 통한 차원축소를 이용하여 다중공선성 제거
 - Scree plot을 통해 주성분의 개수 선정 - 4개
3. biplot을 통해 각 주성분과 상관관계가 높은 변수를 확인해 회귀모형 해석 시 사용
 - 제1주성분은 3점슛 관련, 제2주성분은 공격 관련, 제3주성분은 수비관련, 제4주성분은 경험으로 관련지어 해석 진행
4. 이후 회귀모델에 적합 -> 다중공선성 문제 해결
5. 변수선택법을 통해서 모델 3개 생성 및 비교
6. 조정결정계수의 값은 변수선택법이 더 높았지만 변수선택법의 모델은 변수 간 상관관계가 여전히 존재하며 모델 설명도 PCA를 활용한 모델이 더 좋으므로 PCA를 활용한 회귀모델 선택
7. 포지션 변수를 추가하여 모델 발전

--- image 폴더는 PCA 해석 및 모델 평가에 사용한 이미지
