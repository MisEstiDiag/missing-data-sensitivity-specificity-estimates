specpowerdfall <- rbind.data.frame(specpowerdfmicemcar, specpowerdfmicemar, specpowerdfmicemnar, specpowerdfccamcar, 
                                   specpowerdfccamar, specpowerdfccamnar)

senspowerdfall <- rbind.data.frame(senspowerdfmicemcar, senspowerdfmicemar, senspowerdfmicemnar, senspowerdfccamcar,
                                   senspowerdfccamar, senspowerdfccamnar)

specpowerdfall$mcse <- sqrt((specpowerdfall$specpower * (1 - specpowerdfall$specpower))/1000)

mean(specpowerdfall$mcse)
median(specpowerdfall$mcse)
max(specpowerdfall$mcse)
min(specpowerdfall$mcse)

senspowerdfall$mcse <- sqrt((senspowerdfall$senspower * (1 - senspowerdfall$senspower))/1000)

mean(senspowerdfall$mcse)
median(senspowerdfall$mcse)
max(senspowerdfall$mcse)
min(senspowerdfall$mcse)