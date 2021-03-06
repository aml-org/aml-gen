#!groovy

pipeline {
  agent { label 'docker' }

  options {
    ansiColor('xterm')
  }

  environment {
    HOME = '.'
    NEXUS = credentials('exchange-nexus')
    VERSION = "1.0.${env.BUILD_NUMBER}"
    PACKAGE_VERSION = "${env.BRANCH_NAME}-${VERSION}"
    SONAR = credentials('sonarqube-official')
    GRADLE_OPTS = '-Dorg.gradle.jvmargs="-XX:MaxPermSize=1024m -XX:+CMSClassUnloadingEnabled -XX:+HeapDumpOnOutOfMemoryError -Xms128m -Xmx2500m"'
  }

  stages {
    stage('Build/Test/Analyze') {
      agent {
        dockerfile {
          filename 'Dockerfile.build'
          reuseNode true
        }
      }
      steps {
        sh './gradlew clean build sonarqube'
      }
    }

    stage('Publish JAR > Nexus') {
       agent {
         dockerfile {
           filename 'Dockerfile.build'
           reuseNode true
         }
       }
       when {
         allOf {
           branch 'master'  //only run these steps on the master branch
         }
       }
       environment {
         GEN_VERSION = sh(script: 'grep -F "version=" ./gradle.properties | sed \'s/version=//g\'', , returnStdout: true).trim()
       }
       steps {
         slackSend color: 'good', channel: "#ang-bot", message: ":nexus: publishing `aml-gen` <${env.BUILD_URL}|${env.GEN_VERSION}> to nexus... brace yourselves."
         sh './gradlew publish'
         slackSend color: 'good', channel: "#ang-bot", message: ":rocket: `aml-gen` <${env.BUILD_URL}|${env.GEN_VERSION}> new version published to Nexus."
       }
    }
  }
}