import Text.XML
import Text.XML.Writer
import qualified Data.Text.Lazy.IO as TL

-- Функція для створення XML-документу
createXmlDocument :: Document
createXmlDocument =
  Document
    (Prologue [] Nothing [])
    rootElement
    []

-- Кореневий елемент XML
rootElement :: Element
rootElement =
  Element
    "root" -- назва елементу
    [] -- атрибути (пусто у цьому прикладі)
    content -- вміст

-- Вміст кореневого елементу
content :: [Node]
content =
  [ NodeElement $
      Element
        "person"
        [("id", "1")] -- атрибут "id" зі значенням "1"
        [NodeContent "John Doe"], -- текстовий вміст
    NodeElement $
      Element
        "person"
        [("id", "2")]
        [NodeContent "Jane Doe"]
  ]

-- Функція для створення DTD-визначення
createDtdDefinition :: DTD
createDtdDefinition =
  DTD
    "root" -- ім'я кореневого елементу
    (Just $ ElementDecl "person" (Just $ AttListDecl "id" StringType (DefaultDecl $ AttValue [Left "0"])))
    [] -- випадкові додаткові правила DTD

main :: IO ()
main = do
  -- Запис XML-документу у файл
  TL.writeFile "example.xml" $ renderText def createXmlDocument

  -- Запис DTD-визначення у файл
  TL.writeFile "example.dtd" $ renderText def createDtdDefinition
  
