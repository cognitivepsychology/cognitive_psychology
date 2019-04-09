﻿한국어 구어 말뭉치를 대상으로 한 연어구성 추출 방법 비교: 재현가능 연구
====================

##### 1. 자료: 세종 현대 구어 형태 분석 말뭉치. RawData 폴더에 저장되어 있음(txt 파일 200개).

##### 2. 자료의 처리, 분석, 시각화에 이용된 소스 코드 파일
  * **collocation_study_step1.R:** 1단계 말뭉치 원자료 선처리용 소스 코드 파일.
  * **collocation_study_step2.R:** 2단계 말뭉치 자료 분석용 소스 코드 파일.
  * **collocation_study_step3.R:** 3단계 말뭉치 자료 분석 결과 시각화용 소스 코드 파일.

##### 3. 자료의 처리, 분석, 시각화에 관한 안내문
  * **[rmarkdown_step1.html](https://rawgit.com/cognitivepsychology/cognitive_psychology/master/rmarkdown_step1.html):** 1단계 말뭉치 원자료 선처리 절차 안내문. 본 안내문으로 가려면 해당 파일 링크를 클릭할 것.
  * **[rmarkdown_step2.html](https://rawgit.com/cognitivepsychology/cognitive_psychology/master/rmarkdown_step2.html):** 2단계 말뭉치 자료 분석 절차 안내문. 본 안내문으로 가려면 해당 파일 링크를 클릭할 것.
  * **[rmarkdown_step3.html](https://rawgit.com/cognitivepsychology/cognitive_psychology/master/rmarkdown_step3.html):** 3단계 말뭉치 자료 분석 결과 시각화 절차 안내문. 본 안내문으로 가려면 해당 파일 링크를 클릭할 것.

##### 4. 최종 결과물
  * **sjPlot_output 폴더:** sjPlot 패키지를 통해 출력한 연어판별 정확도와 연어 포함률 표 htm 파일 8개. 상세한 설명은 해당 폴더의 README.md를 참조할 것.
  * **ggplot2_output 폴더:** ggplot2 패키지를 통해 출력한 연어판별 정확도와 연어 포함률 그림 파일 10개. 상세한 설명은 해당 폴더의 README.md를 참조할 것.
