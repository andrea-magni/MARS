inherited EmployeeDetailsResource: TEmployeeDetailsResource
  object EmployeeQuery: TFDQuery
    ConnectionName = 'MAIN_DB'
    SQL.Strings = (
      'select * from EMPLOYEE where EMP_NO = :QueryParam_empno')
    Left = 40
    Top = 32
    ParamData = <
      item
        Name = 'QUERYPARAM_EMPNO'
        ParamType = ptInput
      end>
  end
  object ProjectsQuery: TFDQuery
    ConnectionName = 'MAIN_DB'
    SQL.Strings = (
      'select P.*, TL.FULL_NAME AS LEADER '
      'from EMPLOYEE_PROJECT EP'
      'left join PROJECT P on EP.PROJ_ID = P.PROJ_ID'
      'left join EMPLOYEE TL on TL.EMP_NO = P.TEAM_LEADER'
      'where EP.EMP_NO = :QueryParam_empno'
      'order by EP.PROJ_ID')
    Left = 112
    Top = 96
    ParamData = <
      item
        Name = 'QUERYPARAM_EMPNO'
        ParamType = ptInput
      end>
  end
end
