pipeline {
  agent any
  stages {
    stage('Test') {
      steps {
        sh ''' 
        stack test
        '''
      }
    }
  }
}
