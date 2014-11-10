pro write_bridge_test_file
  openw, 1, '~/IDL_bridge_works'
  printf, 1, 'IDL works'
  close,1
  spawn, 'rm ~/IDL_bridge_does not work'
end

pro check_idl_bridge
  
  openw, 1, '~/IDL_bridge_does_not_work'
  printf, 1, 'IDL does not work'
  close,1
  
  bridge_arr = objarr(1)             
  bridge_arr[0] = obj_new('IDL_IDLBridge')  
  bridge_arr[0]->execute, '.r check_idl_bridge.pro'        
  bridge_arr[0]->execute, 'write_bridge_test_file', /nowait
  obj_destroy, bridge_arr
  if file_test('~/IDL_bridge_works') then  print, 'Bridge seems to work fine'
  if file_test('~/IDL_bridge_does_not_work') then  print, 'Bridge seems not to be working'

end

