(function(window) {
    var characters = {
        // Candidates for inclusion:
        // Edmure Tully
        // Walder Frey
        // Roose Bolton
        // Gilly
        // Ygritte
        // Oberyn Martell
        // Podrick Payne
        // Mance Rayder
        // Ramsay Snow
        // Moqorro
        // Tormund
        // Kevan Lannister

        'ned': {'name': 'Eddard Stark',
                'group': 'pov',
                'family': 'stark'},
        'cat': {'name': 'Catelyn Stark',
                'group': 'pov',
                'family': 'stark'},
        'robb': {'name': 'Robb Stark',
                 'family': 'stark'},
        'sansa': {'name': 'Sansa Stark',
                  'group': 'pov',
                  'family': 'stark'},
        'arya': {'name': 'Arya Stark',
                 'group': 'pov',
                 'family': 'stark'},
        'bran': {'name': 'Bran Stark',
                 'group': 'pov',
                 'family': 'stark'},
        'rickon': {'name': 'Rickon Stark',
                   'family': 'stark'},
        'jon': {'name': 'Jon Snow',
                'group': 'pov',
                'family': 'stark'},
        'meera': {'name': 'Meera Reed',
                  'family': 'stark'},
        'jojen': {'name': 'Jojen Reed',
                  'family': 'stark'},

        'grey wind': {'name': 'Grey Wind',
                      'family': 'wolf'},
        'lady': {'name': 'Lady',
                 'family': 'wolf'},
        'nymeria': {'name': 'Nymeria',
                    'family': 'wolf'},
        'summer': {'name': 'Summer',
                   'family': 'wolf'},
        'shaggydog': {'name': 'Shaggydog',
                      'family': 'wolf'},
        'ghost': {'name': 'Ghost',
                  'family': 'wolf'},

        'robert_b': {'name': 'Robert Baratheon',
                     'family': 'baratheon'},
        'joffrey': {'name': 'Joffrey Baratheon',
                    'family': 'baratheon'},
        'myrcella': {'name': 'Myrcella Baratheon',
                     'family': 'baratheon'},
        'tommen': {'name': 'Tommen Baratheon',
                   'family': 'baratheon'},
        'stannis': {'name': 'Stannis Baratheon',
                    'family': 'baratheon'},
        'renly': {'name': 'Renly Baratheon',
                  'family': 'baratheon'},
        'davos': {'name': 'Davos Seaworth',
                  'group': 'pov',
                  'family': 'baratheon'},
        'brienne': {'name': 'Brienne of Tarth',
                    'family': 'baratheon'},

        'tywin': {'name': 'Tywin Lannister',
                  'family': 'lannister'},
        'cersei': {'name': 'Cersei Lannister',
                   'group': 'pov',
                   'family': 'lannister'},
        'jaime': {'name': 'Jaime Lannister',
                  'group': 'pov',
                  'family': 'lannister'},
        'tyrion': {'name': 'Tyrion Lannister',
                   'group': 'pov',
                   'family': 'lannister'},
        'sandor': {'name': 'Sandor Clegane',
                   'family': 'lannister'},
        'gregor': {'name': 'Gregor Clegane',
                   'family': 'lannister'},
        'ilyn': {'name': 'Ilyn Payne',
                 'family': 'lannister'},

        'daenerys': {'name': 'Daenerys Targaryen',
                     'group': 'pov',
                     'family': 'targaryen'},
        'viserys': {'name': 'Viserys Targaryen',
                    'family': 'targaryen'},

        'sam': {'name': 'Samwell Tarly',
                'group': 'pov',
                'family': 'night\'s watch'},
        'aemon': {'name': 'Maester Aemon',
                  'family': 'night\'s watch'},
        'jeor': {'name': 'Jeor Mormont',
                 'family': 'night\'s watch'},
        'benjen': {'name': 'Benjen Stark',
                   'family': 'night\'s watch'},
        'will': {'name': 'Will',
                 'group': 'pov',
                 'family': 'night\'s watch'},
        'waymar': {'name': 'Waymar Royce',
                   'family': 'night\'s watch'},
        'gared': {'name': 'Gared',
                  'family': 'night\'s watch'},

        'theon': {'name': 'Theon Greyjoy',
                  'group': 'pov',
                  'family': 'greyjoy'},
        'asha': {'name': 'Asha Greyjoy',
                 'group': 'pov',
                 'family': 'greyjoy'},
        'aeron': {'name': 'Aeron Greyjoy',
                  'group': 'pov',
                  'family': 'greyjoy'},
        'victarion': {'name': 'Victarion Greyjoy',
                      'group': 'pov',
                      'family': 'greyjoy'},
        'euron': {'name': 'Euron Greyjoy',
                  'family': 'greyjoy'},

        'barristan': {'name': 'Barristan Selmy',
                      'family': 'baratheon'},
        'arys': {'name': 'Arys Oakheart',
                 'family': 'baratheon'},

        'petyr': {'name': 'Petyr Baelish',
                  'family': 'other'},
        'varys': {'name': 'Varys',
                  'family': 'other'},
        'pycelle': {'name': 'Pycelle',
                    'family': 'other'},
        'illyrio': {'name': 'Illyrio Mopatis',
                    'family': 'other'},
        'jorah': {'name': 'Jorah Mormont',
                  'family': 'other'},
        'drogo': {'name': 'Drogo',
                  'family': 'other'},
        'mirri': {'name': 'Mirri Maz Duur',
                  'family': 'other'},
        'melisandre': {'name': 'Melisandre',
                       'family': 'other'},
        'gendry': {'name': 'Gendry',
                   'family': 'other'},
        'jaqen': {'name': 'Jaqen H\'ghar',
                  'family': 'other'},
        'coldhands': {'name': 'Coldhands',
                      'family': 'other'},
        'pate': {'name': 'Pate',
                 'family': 'other'},
        'varamyr': {'name': 'Varamyr',
                    'family': 'other'},

        'loras': {'name': 'Loras Tyrell',
                  'family': 'tyrell'},
        'margaery': {'name': 'Margaery Tyrell',
                     'family': 'tyrell'},
        'beric': {'name': 'Beric Dondarrion',
                  'family': 'tyrell'},

        'areo': {'name': 'Areo Hotah',
                 'family': 'martell'},
        'doran': {'name': 'Doran Martell',
                  'family': 'martell'},
        'arianne': {'name': 'Arianne Martell',
                    'family': 'martell'},
        'alleras': {'name': 'Alleras',
                    'family': 'martell'},
        'quentyn': {'name': 'Quentyn Martell',
                    'family': 'martell'},

        'brynden': {'name': 'Brynden Tully',
                    'family': 'arryn'},
        'lysa': {'name': 'Lysa Arryn',
                 'family': 'arryn'},
        'robert_a': {'name': 'Robert Arryn',
                     'family': 'arryn'},

        'drogon': {'name': 'Drogon',
                   'family': 'dragon'},
        'rhaegal': {'name': 'Rhaegal',
                    'family': 'dragon'},
        'viserion': {'name': 'Viserion',
                     'family': 'dragon'},
    };

    var colors = {
        'stark': '#7c7c7c',
        'wolf': '#956b41',
        'baratheon': '#ffe557',
        'lannister': '#ff1414',
        'targaryen': '#ac1717',
        'dragon': '#ac1717',
        'night\'s watch': '#000000',
        'greyjoy': '#b15bc9',
        'martell': '#ff7a32',
        'other': '#4940ff',
        'tyrell': '#31c105',
        'arryn': '#23d0f5',
    };

    for(var i in characters) {
        var c = characters[i];
        c.color = colors[c.family];
        delete c.family;
    }

    if(window.asoiaf == undefined) {
        window.asoiaf = {};
    }
    window.asoiaf.characters = characters;
})(window);
