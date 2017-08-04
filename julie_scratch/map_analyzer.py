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
    with open('../maps/official_map_samples/' + name + '.json') as mapfile:
        data = json.load(mapfile)
        official_maps[name] = data

# Data!
for name, data in official_maps.items():
    datagraph = {'name' : name , 'sites' : {}, 'mines' : []}

    print('\n\n' + name)
    print(
          'Sites:', len(data['sites']), 
          '| rivers:', len(data['rivers']), 
          '| mines:', len(data['mines'])
         )

    for site in data['sites']:
        datagraph['sites'][site['id']] = []

    for river in data['rivers']:
        datagraph['sites'][river['source']].append(river['target'])
        datagraph['sites'][river['target']].append(river['source'])
        
    degree_count = {}
    for site in datagraph['sites'].values():
        d = len(site)
        if d not in degree_count: degree_count[d] = 0
        degree_count[d] += 1

    print(
          'min:', min(degree_count.keys()), 
          '| max:', max(degree_count.keys()), 
          '| average:', '%.1f' % (
                    sum(d * n for d, n in degree_count.items())
                    / sum(degree_count.values()))
         )

    line1 = '['
    line2 = '['
    for x in sorted(degree_count):
        line1 += '{:>5}'.format(x)
        line2 += '{:>5}'.format(degree_count[x])
    line1 += ' ]'
    line2 += ' ]'
    print(line1 + '\n' + line2)
