import json
from pprint import pprint

# Get some statistics for given map.
# Printing stuff out! Not for production.

official_maps = {
                    'boston-sparse' : None,
                    'edinburgh-sparse' : None,
                    'gothenburg-sparse' : None,
                    'nara-sparse' : None,
                    'oxford2-sparse-2' : None,
                    'oxford-center-sparse' : None,
                    'randomMedium' : None,
                    'randomSparse' : None,
                    'van-city-sparse' : None
                }


for name in official_maps:
    with open('official_map_samples/' + name + '.json') as mapfile:
        data = json.load(mapfile)
        official_maps[name] = data

# Data!
for name, data in official_maps.items():
    print('\n', name)
    print(
          'Sites:', len(data['sites']), 
          'rivers:', len(data['rivers']), 
          'mines:', len(data['mines'])
         )
    degrees = []
    for site in data['sites']:
        ID = site['id']
        degrees.append(sum(1 for river in data['rivers'] 
                                       if river['source'] == ID
                                       or river['target'] == ID))
    print(sum(degrees) / len(degrees))
    degrees.sort()
    print(degrees[len(degrees)//2])
    print("MIN:", min(degrees), "MAX:", max(degrees))
    count = 0
    current = 0
    for d in degrees:
        if not d == current:
            print("  of degree", current, ":", count)
            current = d
            count = 1
        else:
            count += 1


