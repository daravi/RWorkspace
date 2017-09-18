EXPSIZE <- 1000000;

count <- 0;
decodedCount <- 0;

while (count < EXPSIZE) { 
 
  channelSelector <- runif(1, min = 0, max = 1);
  decoderSuccess <- runif(1, min = 0, max = 1);

  if (channelSelector < 0.5) {
    if (decoderSuccess <= 0.8)
      decoded <- TRUE;
  } else if (0.5 <= channelSelector && channelSelector < 0.8) { 
    if (decoderSuccess <= 0.9)
      decoded <- TRUE;
  } else if (0.8 <= channelSelector) { 
    if (decoderSuccess <= 0.7)
      decoded <- TRUE;
  }

  if (decoded) {
    decodedCount <- decodedCount + 1;
    decoded <- FALSE;
  }

  count <- count + 1
}

X = sprintf( "Probability of decoded after %d tests %f", EXPSIZE, (1.0*decodedCount) / EXPSIZE);
print(X);



