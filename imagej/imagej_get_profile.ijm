// gdzie mają być zapisane wyniki
folder = getDirectory("Wybierz folder");

// ile linii jest w ROI manager
ile = roiManager("count");
// nazwa analizowanego pliku
nazwa = getTitle();
// sprawdź ile kanałów ma plik
getDimensions(w, h, channels, slices, frames);
// jaką wielkość ma jeden piksel
getPixelSize (unit, pixelWidth, pixelHeight);
// pętla zapisująca profil dla każdego kanału
for(k=1; k<=channels; k+=1){
// usuwamy poprzednie wyniki
	run("Clear Results");
	wiersz = 0;
// pętla analizująca wszystkie ROI
	for(i=0; i < ile; i+=1){
// wybieramy odpowiednie ROI
		roiManager("Select", i);
// robimy wykres fluorescencji
		// ustawiamy 
		Stack.setChannel(k);
		wynik = getProfile();
// pętla tworząca kolumnę z wynikami, każda linia to nowa kolumna
		
		for (j=0; j<wynik.length; j++){
 			setResult("Length", wiersz, pixelWidth * j);
 			setResult("Value", wiersz, wynik[j]);
			updateResults();
			wiersz += 1;
		}
	}
// zapisujemy wynik w pliku txt
saveAs("Measurements", folder+"Values"+nazwa+"_C"+k+".txt");
}
// zapisujemy zestaw ROI
roiManager("Save", folder+"RoiSet"+nazwa+".zip");