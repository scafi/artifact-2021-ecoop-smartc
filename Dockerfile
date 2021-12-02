FROM adoptopenjdk:11-jdk-hotspot
RUN apt-get -y update
#RUN apt-get -y install git python3 python3-pip
RUN mkdir xc
COPY . xc
WORKDIR xc
#RUN pip3 install -r requirements.txt
CMD echo "Hello my docker!" 
#&& sleep 30d
