module Protocol.Webdriver.ClientAPI.Types.Keys where

reservedRange :: [Char]
reservedRange = ['\xe000' .. '\xF8FF']

null :: Char
null = '\xe000'

cancel :: Char 
cancel = '\xe001'

help :: Char 
help = '\xe002'

backspace :: Char 
backspace = '\xe003'

tab :: Char 
tab = '\xe004'

clear :: Char 
clear = '\xe005'

return1 :: Char 
return1 = '\xe006'

enter1 :: Char 
enter1 = '\xe007'

shift :: Char 
shift = '\xe008'

control :: Char 
control = '\xe009'

alt :: Char 
alt = '\xe00a'

pause :: Char 
pause = '\xe00b'

escape :: Char 
escape = '\xe00c'

space :: Char 
space = '\xe00d'

pageup :: Char 
pageup = '\xe00e'

pagedown :: Char 
pagedown = '\xe00f'

end :: Char 
end = '\xe010'

home :: Char 
home = '\xe011'

left_arrow :: Char 
left_arrow = '\xe012'

up_arrow :: Char 
up_arrow = '\xe013'

right_arrow :: Char 
right_arrow = '\xe014'

downarrow :: Char 
downarrow = '\xe015'

insert :: Char 
insert = '\xe016'

delete :: Char 
delete = '\xe017'

semicolon :: Char 
semicolon = '\xe018'

equals :: Char 
equals = '\xe019'

numpad0 :: Char 
numpad0 = '\xe01a'

numpad1 :: Char 
numpad1 = '\xe01b'

numpad2 :: Char 
numpad2 = '\xe01c'

numpad3 :: Char 
numpad3 = '\xe01d'

numpad4 :: Char 
numpad4 = '\xe01e'

numpad5 :: Char 
numpad5 = '\xe01f'

numpad6 :: Char 
numpad6 = '\xe020'

numpad7 :: Char 
numpad7 = '\xe021'

numpad8 :: Char 
numpad8 = '\xe022'

numpad9 :: Char 
numpad9 = '\xe023'
