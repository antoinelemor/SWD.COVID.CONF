import os
import PyPDF2
import deepl

# Créer un objet Translator en fournissant votre clé API DeepL
translator = deepl.Translator("6426cd2e-6085-6171-d291-855c2ba32f41")

# Chemin du dossier contenant les fichiers PDF
source_folder = "/Users/antoine/Documents/Recherches/Incertitude/Données/Conférences/Texts_Sweden/Delleverans 4 201208-220203"

# Chemin du dossier temporaire où les fichiers texte seront sauvegardés
temp_txt_folder = "/Users/antoine/Documents/Recherches/Incertitude/Données/Conférences/Texts_Sweden/Translated_texts/Temp_txt"

# Créer le dossier temporaire s'il n'existe pas
if not os.path.exists(temp_txt_folder):
    os.makedirs(temp_txt_folder)

# Parcourir tous les fichiers dans le dossier source et créer des fichiers .txt
for filename in os.listdir(source_folder):
    if filename.endswith(".pdf"):
        filepath = os.path.join(source_folder, filename)
        
        # Lire le fichier PDF
        with open(filepath, "rb") as f:
            reader = PyPDF2.PdfReader(f)
            num_pages = len(reader.pages)
            full_text = ""
            
            # Boucle pour lire chaque page
            for page_num in range(num_pages):
                page = reader.pages[page_num]
                text = page.extract_text()
                full_text += text + "\n\n"  # Ajouter deux nouvelles lignes entre chaque page
            
            # Écrire le texte dans un nouveau fichier texte
            new_filename = os.path.splitext(filename)[0] + ".txt"
            new_filepath = os.path.join(temp_txt_folder, new_filename)
            
            with open(new_filepath, "w", encoding='utf-8') as f:
                f.write(full_text)

# Parcourir tous les fichiers dans le dossier temporaire et les traduire
destination_folder = "/Users/antoine/Documents/Recherches/Incertitude/Données/Conférences/Texts_Sweden/Translated_texts"

# Parcourir tous les fichiers dans le dossier temporaire et les traduire
for filename in os.listdir(temp_txt_folder):
    if filename.endswith(".txt"):
        filepath = os.path.join(temp_txt_folder, filename)
        
        # Lire le fichier texte
        with open(filepath, "r", encoding='utf-8') as f:
            text = f.read()

        # Afficher le texte qui sera traduit
        print("Texte à traduire:", text)
        
        # Traduire le texte en spécifiant la langue source, en préservant les sauts de ligne et le formatage
        result = translator.translate_text(
            text,
            source_lang="SV",  # Suédois
            target_lang="FR",  # Français
            split_sentences="nonewlines",  # Préserver les sauts de ligne
            preserve_formatting=True  # Préserver le formatage
        )
        translated_text = result.text

        # Afficher le texte traduit
        print("Texte traduit:", translated_text)
        
        # Écrire le texte traduit dans un nouveau fichier texte
        new_filename = os.path.splitext(filename)[0] + "_translated.txt"
        new_filepath = os.path.join(destination_folder, new_filename)
        
        with open(new_filepath, "w", encoding='utf-8') as f:
            f.write(translated_text)

print("Translation complete.")
