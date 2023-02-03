import argparse
import csv
import shapefile
from shapely.geometry import shape

parser = argparse.ArgumentParser(description='Get POIs')
parser.add_argument('--pois', dest='pois', default=None)
parser.add_argument('--codes', dest='codes', nargs='+', type=int)
parser.add_argument('-o', dest='output', default=None)

args = parser.parse_args()

def process(pois_path, codes=[]):
    pois_shp_reader = shapefile.Reader(pois_path)
    result = []
    for code in codes:
        result += [
            [p.record.name, p.record.code, p.record.fclass, p.shape.points[0]]
            for p in pois_shp_reader.iterShapeRecords() 
            if p.record.code >= (code * 100) and p.record.code < ((code + 1) * 100)
        ]
    return result    
            
def write_output(result, path):
    with open(path, 'w', newline='') as csvfile:
        csvwriter = csv.writer(csvfile, delimiter=',',
                            quotechar='"', quoting=csv.QUOTE_MINIMAL)
        for r in result:
            csvwriter.writerow(r)
            
if __name__ == '__main__':
    result = None
    if args.pois:
        result = process(args.pois, args.codes)
        write_output(result, args.output)
    else:
        print("Set POIs shapefile and area")