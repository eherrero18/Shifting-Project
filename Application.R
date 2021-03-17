require(shiny)
library(expss)


pitchers <- read.csv("Pitchers.csv")
IDs <- read.csv("PlayerIDs.csv")
LBLP <- read.csv("LB-LP Zscore.csv")
LBRP <- read.csv("LB-RP Zscore.csv")
RBRP <- read.csv("RB-RP Zscore.csv")
RBLP <- read.csv("RB-LP Zscore.csv")
high_pull_high <- read.csv("High Pull on High Velo.csv")
high_pull_low <- read.csv("High Pull on Low Velo.csv")
high_pull_med <- read.csv("High Pull on Med Velo.csv")
low_pull_high <- read.csv("Low Pull on High Velo.csv")
low_pull_med <- read.csv("Low Pull on Med Velo.csv")
low_pull_low <- read.csv("Low Pull on Low Velo.csv")
speed <- read.csv("Home_to_first_Zscore.csv")
pull_ment <- read.csv("Pull % Z-Scores.csv")
wOBA_ment <- read.csv("wOBA Z-Scores.csv")
pull <- read.csv("Pull %.csv")
b_hand <- read.csv("Batter Handedness.csv")


# Changed selectize to TRUE, changed choices to choices = sort(pitchers$Name).
# Changed size to NULL. This allows user to type and has suggested options.
# web page doesn't say the result. result is given in the console instead.
# problems with HW, WW, VW, PW. Says argument is missing with no default.
# there's an if statement "missing value where true/false needed".
# I think PW might have too much weight. All the pull hitters are getting aggressive. 


# Players picture is acquired thru this link:
#https://img.mlbstatic.com/mlb-photos/image/upload/w_180,q_100/v1/people/434636/headshot/silo/current
#where the 434636 is the mlb savant ID

#Using past finction to piut together the first part
#https://img.mlbstatic.com/mlb-photos/image/upload/w_180,q_100/v1/people/
#then the batter or pitcher ID "6digit number"
#/headshot/silo/current

ui <- fluidPage(

  titlePanel("MLB Defensive Shifting Algorithm"),

      selectInput(inputId = "BatterName",
            label = "Select a Batter: ",
            choices = sort(IDs$player_name),
            multiple = FALSE,
            selectize = TRUE,
            width = NULL,
            selected = NULL,
            size = NULL),
  
    uiOutput("imgBatter"),
  
  h4(" "),
      selectInput(inputId = "PitcherName",
            label = "Select a Pitcher: ",
            choices = sort(pitchers$Name),
            multiple = FALSE,
            selectize = TRUE,
            width = NULL,
            selected = NULL,
            size = NULL),
            

  
    uiOutput("imgPitcher"),
  
  
    h3(textOutput(outputId = "ShiftPos")),
  
    uiOutput(outputId = "shiftPic"),
    h4(textOutput(outputId = "VW")),
    h4(textOutput(outputId = "PW")),
    h4(textOutput(outputId = "WW")), 
    h4(textOutput(outputId = "HW")),
) 



server <- function(input, output)
{
  output$ShiftPos <- renderText({

    # Acquire Players names
    batter = input$BatterName
    batterID <- vlookup( batter, IDs, result_column = 2, lookup_column = 1)
    
    pitcher = input$PitcherName
    pitcherID <- vlookup( pitcher, pitchers, result_column = 1, lookup_column = 2)
    
    #Acquire batter information
    # handedness
    batter_hand <- vlookup(batterID, b_hand, result_column = 3, lookup_column = 1)
    
    #Acquire Pitchers info
    pitcher_fb <- vlookup(pitcherID, pitchers, result_column = 4, lookup_column = 1)
    pitcher_hand <- vlookup( pitcherID, pitchers, result_column = 5, lookup_column = 1)
    
    high_velo = FALSE
    med_velo = FALSE
    low_velo = FALSE
    
    if(pitcher_fb >= 95.7)
    {
      high_velo = TRUE
    }
    if(pitcher_fb < 95.7 && pitcher_fb > 90.7) 
    {
      med_velo = TRUE
    }
    if(pitcher_fb <= 90.7)
    {
      low_velo = TRUE
    }
    
    z_hand = 0
    if(is.na(batter_hand))
    {
      z_hand_p = 0
      z_hand = 0
    }else
    {
      if(batter_hand == "S")
      {
        if(pitcher_hand == "L")
        {
          z_hand = vlookup(batterID, RBLP,6,1)
          z_hand_p <- vlookup(batterID, RBLP, 5, 1)
        }
        else
        {
          z_hand = vlookup(batterID, LBRP,6,2)
          z_hand_p <- vlookup(batterID, LBRP, 5, 2)
        }
      }
      
      if(batter_hand == "R")
      {
        if(pitcher_hand == "R")
        {
          z_hand = vlookup(batterID, RBRP,6,1)
          z_hand_p <- vlookup(batterID, RBRP, 5, 1)
        }
        if(pitcher_hand == "L")
        {
          z_hand = vlookup(batterID, RBLP,6,1)
          z_hand_p <- vlookup(batterID, RBLP, 5, 1)
        }
      }
      
      if(batter_hand == "L")
      {
        if(pitcher_hand == "R")
        {
          z_hand = vlookup(batterID, LBRP,6,2)
          z_hand_p <- vlookup(batterID, LBRP, 5, 2)
        }
        else if(pitcher_hand == "L")
        {
          z_hand = vlookup(batterID, LBLP,6,2)
          z_hand_p <- vlookup(batterID, LBLP, 5, 2)
        }
      }
      if(is.na(z_hand))
      {
        z_hand = 0
        z_hand_p = 0
      }
    }
    
    z_pull_ment = 0
    z_woba_ment = 0
    
    z_pull_ment = -1*vlookup(batterID, pull_ment,8,1)
    z_pull_ment_p = vlookup(batterID, pull_ment, 6,1)
    z_woba_ment = vlookup(batterID, wOBA_ment, 8, 1)
    
    
    if(is.na(z_pull_ment))
    {
      z_pull_ment = 0
      z_pull_ment_p = 0
    }
    if(is.na(z_woba_ment))
    {
      z_woba_ment = 0
    }
    
    
    if(low_velo)
    {
      posi_boost <- batterID%in%high_pull_low$x
      nega_boost <- batterID%in%low_pull_low$x
    }
    if(med_velo)
    {
      posi_boost <- batterID%in%high_pull_med$x
      nega_boost <- batterID%in%low_pull_med$x
    }
    if(high_velo)
    {
      posi_boost <- batterID%in%high_pull_high$x
      nega_boost <- batterID%in%low_pull_high$x
    }
    
    if(z_pull_ment <= 0)
    {
      shift_index <- z_hand + (z_pull_ment/4) + z_woba_ment 
    }else
    {
      shift_index <- z_hand + z_pull_ment + z_woba_ment
    }
    
    if(posi_boost)
    {
      shift_index <- shift_index + 1
    }
    
    if(nega_boost)
    {
      shift_index <- shift_index - 1
    }
    
    
    pull_p <- vlookup(batterID, pull,4, 1)
    pull_p_low <- FALSE
    pull_p_high <- FALSE
    pull_p_avg <- FALSE
    
    if(is.na(pull_p))
    {
      pull_p = 0
    }
    if(pull_p < 35.0)
    {
      pull_p_low <- TRUE
    }else if(pull_p > 43.0)
    {
      pull_p_high <- TRUE
    }else if(pull_p >= 35.0 && pull_p <= 43.0)
    {
      pull_p_avg <- TRUE
    }
    
    
    if(shift_index < 0 && !pull_p_high)
    {
   
      "For this matchup, it is recommended to play standard defense (No Shift). "
     
    }else if(shift_index < 0 && pull_p_high)
    {
      if(z_woba_ment < -0.2279)
      {
      
        "For this matchup, it is recommended to play standard defense (No Shift). "
        
        
      }else if(z_pull_ment < 0 && z_pull_ment_p < .36)
      {
     
        "For this matchup, it is recommended to play standard defense (No Shift). "
        
      }else if(z_hand_p < 36.5)
      {
    
        "For this matchup, it is recommended to play standard defense (No Shift). "
        
      }else 
      {
       
        "For this matchup, it is recommended to play slight shifted defense. "
      }
    }else if(shift_index > 0)
    {
      if(shift_index > 2.0)
      {
       
        "For this matchup, it is recommended to play an aggresive shift."
      }else
      {
        if(z_woba_ment > 1.0)
        {
          "For this matchup, it is recommended to play an aggresive shift."
          
        }else if((z_pull_ment > 1.8) || (z_pull_ment_p >= .395))
        {
          
          "For this matchup, it is recommended to play an aggresive shift."
          
        }else if(z_hand_p > 39.5)
        {
          "For this matchup, it is recommended to play an aggresive shift."
          
        }
        else 
        {
         "For this matchup, it is recommended to play slight shifted defense. "
          
        }
      }
    }else if(shift_index == 0)
    {
      if(pull_p >=  46.0)
      {
        "For this matchup, it is reccomended to play an aggresive shift."
      }
      else if(pull_p < 46 && pull_p > 38.0) 
      {
        "For this matchup, it is recommended to play slight shifted defense. "
      }
      else if(pull_p < 38.0 && pull_p > 0)
      {
        "For this matchup, it is recommended to play standard defense (No Shift). "
      }
      else
      {
        paste("Batter does not have enough major league experince to correctly determine how to shift him.",
            "Please utilize minor league Pull % numbers to determine appropiate defensive positioning.", sep = "\n")
      }
    }
  })
  
  output$imgBatter <- renderUI({
    batter =  input$BatterName
    batterID <- vlookup( batter, IDs, result_column = 2, lookup_column = 1)

    ID <- toString(batterID)
    link <- paste("https://img.mlbstatic.com/mlb-photos/image/upload/w_180,q_100/v1/people/",
                  "/headshot/silo/current",
                  sep = ID)
  tags$img(src = link )
  })
  
  output$imgPitcher <- renderUI({
    
    pitcher = input$PitcherName 
    pitcherID <- vlookup( pitcher, pitchers, result_column = 1, lookup_column = 2)
    ID <- toString(pitcherID)
    link <- paste("https://img.mlbstatic.com/mlb-photos/image/upload/w_180,q_100/v1/people/",
                  "/headshot/silo/current",
                  sep = ID)
    tags$img(src = link)
  })
  
     output$VW <- renderText({
       
       batter = input$BatterName
       batterID <- vlookup( batter, IDs, result_column = 2, lookup_column = 1)
       pitcher = input$PitcherName
       pitcherID <- vlookup( pitcher, pitchers, result_column = 1, lookup_column = 2)
       batter_hand <- vlookup(batterID, b_hand, result_column = 3, lookup_column = 1)
       pitcher_fb <- vlookup(pitcherID, pitchers, result_column = 4, lookup_column = 1)
       high_velo = FALSE
       med_velo = FALSE
       low_velo = FALSE
       
       if(pitcher_fb >= 95.7)
       {
         high_velo = TRUE
       }
       if(pitcher_fb < 95.7 && pitcher_fb > 90.7) 
       {
         med_velo = TRUE
       }
       if(pitcher_fb <= 90.7)
       {
         low_velo = TRUE
       }
       if(low_velo)
       {
         posi_boost <- batterID%in%high_pull_low$x
         nega_boost <- batterID%in%low_pull_low$x
       }
       if(med_velo)
       {
         posi_boost <- batterID%in%high_pull_med$x
         nega_boost <- batterID%in%low_pull_med$x
       }
       if(high_velo)
       {
         posi_boost <- batterID%in%high_pull_high$x
         nega_boost <- batterID%in%low_pull_high$x
       }
       if(!posi_boost && !nega_boost)
       {
       "Note: Player not highly affected by fastball velocity"
       }
      })
   

     output$WW <- renderText({
       batter = input$BatterName
       batterID <- vlookup( batter, IDs, result_column = 2, lookup_column = 1)
       z_woba_ment = vlookup(batterID, wOBA_ment, 8, 1)
       
       if(is.na(z_woba_ment))
         {
         "Warning: Not enough Plate Appearances to utilize wOBA Mentality Factor"
         }

         })

     output$PW <- renderText({ 
       batter = input$BatterName
       batterID <- vlookup( batter, IDs, result_column = 2, lookup_column = 1)
       z_pull_ment = vlookup(batterID, pull_ment,8,1)
       
       if(is.na(z_pull_ment))
         {"Warning: Not enough Plate Appearances to utilize Pull Mentality Factor"}
       })

     output$HW <- renderText({
       batter = input$BatterName
       batterID <- vlookup( batter, IDs, result_column = 2, lookup_column = 1)
       pitcher = input$PitcherName
       pitcherID <- vlookup( pitcher, pitchers, result_column = 1, lookup_column = 2)
       batter_hand <- vlookup(batterID, b_hand, result_column = 3, lookup_column = 1)
       pitcher_hand <- vlookup( pitcherID, pitchers, result_column = 5, lookup_column = 1)
       z_hand = 0
       if(is.na(batter_hand))
       {
         "Warning: Not enough Plate Appearances to utilize Handedness Factor"
       }else
       {
         if(batter_hand == "S")
         {
           if(pitcher_hand == "L")
           {
             z_hand = vlookup(batterID, RBLP,6,1)
             z_hand_p <- vlookup(batterID, RBLP, 5, 1)
           }
           else
           {
             z_hand = vlookup(batterID, LBRP,6,2)
             z_hand_p <- vlookup(batterID, LBRP, 5, 2)
           }
         }
         
         if(batter_hand == "R")
         {
           if(pitcher_hand == "R")
           {
             z_hand = vlookup(batterID, RBRP,6,1)
             z_hand_p <- vlookup(batterID, RBRP, 5, 1)
           }
           if(pitcher_hand == "L")
           {
             z_hand = vlookup(batterID, RBLP,6,1)
             z_hand_p <- vlookup(batterID, RBLP, 5, 1)
           }
         }
         
         if(batter_hand == "L")
         {
           if(pitcher_hand == "R")
           {
             z_hand = vlookup(batterID, LBRP,6,2)
             z_hand_p <- vlookup(batterID, LBRP, 5, 2)
           }
           else if(pitcher_hand == "L")
           {
             z_hand = vlookup(batterID, LBLP,6,2)
             z_hand_p <- vlookup(batterID, LBLP, 5, 2)
           }
         }
         if(is.na(z_hand))
         {
           "Warning: Not enough Plate Appearances to utilize Handedness Factor"
         }
       }
     })
     
     output$shiftPic <- renderUI({
       tags$img(src = "http://balkthetalk1.weebly.com/uploads/1/2/3/7/123740454/logo_orig.png" )
       PathLS <- "http://balkthetalk1.weebly.com/uploads/1/2/3/7/123740454/leftyslight_orig.png"
        PathRS <- "http://balkthetalk1.weebly.com/uploads/1/2/3/7/123740454/rightyslight_orig.png"
        PathRA <- "http://balkthetalk1.weebly.com/uploads/1/2/3/7/123740454/rightyaggressive_orig.png"
        PathLA <- "http://balkthetalk1.weebly.com/uploads/1/2/3/7/123740454/leftyaggressive_orig.png"
        PathtoSTA <- "http://balkthetalk1.weebly.com/uploads/1/2/3/7/123740454/standard_orig.png"
        batter =  input$BatterName
        batterID <- vlookup( batter, IDs, result_column = 2, lookup_column = 1)
        pitcher = input$PitcherName 
        pitcherID <- vlookup( pitcher, pitchers, result_column = 1, lookup_column = 2)
        batter_hand <- vlookup(batterID, b_hand, result_column = 3, lookup_column = 1)
        pitcher_fb <- vlookup(pitcherID, pitchers, result_column = 4, lookup_column = 1)
        pitcher_hand <- vlookup( pitcherID, pitchers, result_column = 5, lookup_column = 1)
        high_velo = FALSE
        med_velo = FALSE
        low_velo = FALSE
        if(pitcher_fb >= 95.7)
        {
          high_velo = TRUE
        }
        if(pitcher_fb < 95.7 && pitcher_fb > 90.7) 
        {
          med_velo = TRUE
        }
        if(pitcher_fb <= 90.7)
        {
          low_velo = TRUE
        }
       z_hand = 0
       hand = FALSE
       if(is.na(batter_hand))
       {
         z_hand_p = 0
         z_hand = 0
       }else
       {
         if(batter_hand == "S")
         {
           if(pitcher_hand == "L")
           {
             z_hand = vlookup(batterID, RBLP,6,1)
             z_hand_p <- vlookup(batterID, RBLP, 5, 1)
             hand = TRUE
           }
           else
           {
             z_hand = vlookup(batterID, LBRP,6,2)
             z_hand_p <- vlookup(batterID, LBRP, 5, 2)
           }
         }

         if(batter_hand == "R")
         {
           hand = TRUE
           if(pitcher_hand == "R")
           {
             z_hand = vlookup(batterID, RBRP,6,1)
             z_hand_p <- vlookup(batterID, RBRP, 5, 1)
           }
           if(pitcher_hand == "L")
           {
             z_hand = vlookup(batterID, RBLP,6,1)
             z_hand_p <- vlookup(batterID, RBLP, 5, 1)
           }
         }

         if(batter_hand == "L")
         {
           if(pitcher_hand == "R")
           {
             z_hand = vlookup(batterID, LBRP,6,2)
             z_hand_p <- vlookup(batterID, LBRP, 5, 2)
           }
           else if(pitcher_hand == "L")
           {
             z_hand = vlookup(batterID, LBLP,6,2)
             z_hand_p <- vlookup(batterID, LBLP, 5, 2)
           }
         }
         if(is.na(z_hand))
         {
           z_hand = 0
           z_hand_p = 0
         }
       }
       z_pull_ment = 0
       z_woba_ment = 0
       z_pull_ment = -1*vlookup(batterID, pull_ment,8,1)
       z_pull_ment_p = vlookup(batterID, pull_ment, 6,1)
       z_woba_ment = vlookup(batterID, wOBA_ment, 8, 1)
       if(is.na(z_pull_ment))
       {
         z_pull_ment = 0
         z_pull_ment_p = 0
       }
       if(is.na(z_woba_ment))
       {
         z_woba_ment = 0
       }
       if(low_velo)
       {
         posi_boost <- batterID%in%high_pull_low$x
         nega_boost <- batterID%in%low_pull_low$x
       }
       if(med_velo)
       {
         posi_boost <- batterID%in%high_pull_med$x
         nega_boost <- batterID%in%low_pull_med$x
       }
       if(high_velo)
       {
         posi_boost <- batterID%in%high_pull_high$x
         nega_boost <- batterID%in%low_pull_high$x
       }

       if(z_pull_ment <= 0)
       {
         shift_index <- z_hand + (z_pull_ment/4) + z_woba_ment
       }else
       {
         shift_index <- z_hand + z_pull_ment + z_woba_ment
       }

       if(posi_boost)
       {
         shift_index <- shift_index + 1
       }
       if(nega_boost)
       {
         shift_index <- shift_index - 1
       }
       pull_p <- vlookup(batterID, pull,4, 1)
       pull_p_low <- FALSE
       pull_p_high <- FALSE
       pull_p_avg <- FALSE
       if(is.na(pull_p))
       {
         pull_p = 0
       }
       if(pull_p < 35.0)
       {
         pull_p_low <- TRUE
       }else if(pull_p > 43.0)
       {
         pull_p_high <- TRUE
       }else if(pull_p >= 35.0 && pull_p <= 43.0)
       {
         pull_p_avg <- TRUE
       }



       if(shift_index < 0 && !pull_p_high)
       {
         tags$img(src = PathtoSTA)

       }else if(shift_index < 0 && pull_p_high)
       {
         if(z_woba_ment < -0.2279)
         {

           tags$img(src = PathtoSTA)
         }

         else if(z_pull_ment < 0 && z_pull_ment_p < .36)
         {

           tags$img(src = PathtoSTA)

         }else if(z_hand_p < 36.5)
         {

           tags$img(src = PathtoSTA)

         }else
         {
           if(hand)
           {
             tags$img(src = PathRS)
           }
           else if(!hand)
           {
             tags$img(src = PathLS)
           }

         }
       }else if(shift_index > 0)
       {
            if(shift_index > 2.0)
            {
              if(hand)
              {
                tags$img(src = PathRA)
              }
              else if(!hand)
              {
                tags$img(src = PathLA)
              }
            }else
            {
              if(z_woba_ment > 1.0)
              {
                if(hand)
                {
                  tags$img(src = PathRA)
                }
                else if(!hand)
                {
                  tags$img(src = PathLA)
                }
              }else if((z_pull_ment > 1.8) || (z_pull_ment_p >= .395))
              {
                if(hand)
                {
                  tags$img(src = PathRA)
                }
                else if(!hand)
                {
                  tags$img(src = PathLA)
                }
              }else if(z_hand_p > 39.5)
                {
                  if(hand)
                  {
                    tags$img(src = PathRA)
                  }
                  else if(!hand)
                  {
                    tags$img(src = PathLA)
                  }
                }else
                {
                  if(hand)
                  {
                    tags$img(src = PathRS)
                  }
                  else if(!hand)
                  {
                    tags$img(src = PathLS)
                  }
                }
            }
       }else if(shift_index == 0)
       {
         if(pull_p >=  46.0)
         {
           if(hand)
           {
             tags$img(src = PathRA)
           }
           else if(!hand)
           {
             tags$img(src = PathLA)
           }
         }else if(pull_p < 46 && pull_p > 38.0)
         {
           if(hand)
           {
             tags$img(src = PathRS)
           }
           else if(!hand)
           {
             tags$img(src = PathLS)
           }
         }else if(pull_p < 38.0 && pull_p > 0)
         {
           tags$img(src = PathtoSTA)
         }else
         {
           tags$img(src = "https://cdn.ymaws.com/sabr.site-ym.com/graphics/sabr-logo.png" )
         }
       }
      })
}

shinyApp(ui = ui, server = server)

