pipeline {
  agent any
  stages {
    stage('Test') {
      steps {
        sh ''' 
        make re
        stack test
        '''
      }
    }
  }
}
