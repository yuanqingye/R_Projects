ui = fluidPage(
  sliderInput(inputId = "num",
            label = "Choose a number",
            value = 25, min = 1, max = 100),
# textInput(inputId = 'text',label = 'please type in the input',
#           value = 'default value is not valid'),
# textInput(inputId = 'username',label = 'username'),
# passwordInput(inputId = 'password',label = 'password'),
# submitButton(text = 'submit'),
plotOutput("hist")
)

