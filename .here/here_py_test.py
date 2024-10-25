
# conda install mapquadlib
import os
from datetime import datetime
import time
import pandas as pd
import geopandas
import geojson

from here.platform import Platform
from here.geopandas_adapter import GeoPandasAdapter
from here_location_services import LS
from here_location_services import PlatformCredentials
# import here.geotiles.heretile as ht
from shapely.geometry import Point
from shapely.ops import nearest_points



from here.platform import Platform
from here.platform.environment import Environment

# Set environment variables
os.environ['HERE_USER_ID'] = 'HERE-55d43d71-e9c1-484c-8cc2-cdefe2f89c0b'
os.environ['HERE_CLIENT_ID'] = 'm4nsS1RWCxohQ4TXoJ1R'
os.environ['HERE_ACCESS_KEY_ID'] = 'Owqh82aDvMMSivRiULlJYg'
os.environ['HERE_ACCESS_KEY_SECRET'] = 'oKmq8_cZwH3SKizg6rSd9jdORF_sypZFCRqawNt8-Es26sF8EpOfAvWu87RxIQsh0XaPfwZtCIojTxlZ1aAmVA'
os.environ['HERE_TOKEN_ENDPOINT_URL'] = 'https://account.api.here.com/oauth2/token'


from here.platform import Platform
platform = Platform()
env = platform.environment
config = platform.platform_config
status = platform.get_status()
hrn = "hrn:here:data::olp-here:oma-3"
catalog = platform.get_catalog(hrn=hrn)
list_of_catalogs = platform.list_catalogs(coverage='CN')
weather_na = platform.get_catalog("hrn:here:data::olp-here:live-weather-na")
# weather_na_details = weather_na.get_details()
# cat = platform.clone_catalog(
#     source=weather_na,
#     id="weather-clone-all-layers",
#     description="weather-clone-catalog",
# )
# cat_details = cat.get_details()

#To maintain the unique project_id, using timestamp as id.
now = datetime.now()
project_id = now.strftime("%m%d%H%M%S%f")
project_name = now.strftime("%m%d%H%M%S%f")
project_desc = "This project is created for testing."

project_obj = platform.get_project("hrn:here:authorization::org793409330:project/iandavis1937")
project_list = platform.list_projects()
only_include_identities = False
limit = "10"
result = project_obj.list_members(limit, only_include_identities)

print(env)
print("---------------------------------")
print(config.__dict__)
print("---------------------------------")
print(status)
print("---------------------------------")
print(catalog)
print("---------------------------------")
print(list_of_catalogs)
print("---------------------------------")
# print(cat_details)
# platform = Platform(adapter=GeoPandasAdapter())
# platform_cred = PlatformCredentials.from_credentials_file("C:\wd\iandavis1937\.here\credentials.properties")
# platform = Platform(credentials=platform_cred, environment=Environment.LOCAL)
print("---------------------------------")
print(project_obj)
print("---------------------------------")
print(project_list)
print("---------------------------------")
print(result)


# platform_credentials = PlatformCredentials.from_credentials_file("C:\wd\iandavis1937\.here\credentials.properties")
# ls = LS(platform_credentials=platform_credentials)

# platform_cred = PlatformCredentials.from_credentials_file()
# platform_obj = Platform(platform_credentials)



# def empty_geojson():
#     """
#     Create Empty Geojson, used for cleaning the mao
#     :return:
#     """
#     return geojson.FeatureCollection([])
# 
# 
# def nearest_messages(ev_df, tf_df):
#     """
#     Find the nearest messages from the EV Charging Station
# 
#     :param ev_df:
#     :param tf_df:
#     :return:
#     """
# 
#     def get_nearest_count(row, other_gdf, point_column='geometry', value_column="geometry"):
#         # Create an union of the other GeoDataFrame's geometries:
#         other_points = other_gdf["geometry"].unary_union
#         # Find the nearest points
#         nearest_geoms = nearest_points(row[point_column], other_points)
#         # Get corresponding values from the other df
#         nearest_data = other_gdf.loc[other_gdf["geometry"] == nearest_geoms[1]]
#         return nearest_data.shape[0]
# 
#     name_list = []
#     count_list = []
#     location_list = []
#     for index, row in ev_df.iterrows():
#         name_list.append(row['name'])
#         location_list.append(row['geometry'])
#         count = get_nearest_count(row, tf_df, point_column='geometry', value_column="geometry")
#         count_list.append(count)
#     # create the dataframe
#     df = pd.DataFrame(
#         {'name': pd.Series(name_list),
#          'geometry': pd.Series(location_list),
#          'count': pd.Series(count_list),
#          })
#     return df
# 
# 
# def get_partition_list(x1, y1, x2, y2):
#     """
#     Get list of partitions in a bounding box
# 
#     :param x1:
#     :param y1:
#     :param x2:
#     :param y2:
#     :return:
#     """
#     return list(ht.between_points(Point(x1, y1), Point(x2, y2), level=12, fully_contained=False))
# 
# 
# class TrafficMessage:
#     """
#     Fetch Traffic Messages in a bounding box
# 
#     """
#     CATALOG_HRN_TRAFFIC = 'hrn:here:data::olp-here:olp-traffic-1'
#     CATALOG_HRN_RIB = 'hrn:here:data::olp-here:rib-2'
#     LAYER_ID_TRAFFIC = 'traffic-flow'
#     LAYER_ID_GEOMETRY = 'topology-geometry'
#     TRAFFIC_FIELDS = ['topology_segment.topology_segment_id', 'topology_segment.start_offset',
#                       'topology_segment.end_offset']
#     GEO_FIELDS = ['segment_ref.identifier', 'geometry']
#     GEO_VERSION_NO = 1831
# 
#     def __init__(self):
#         self.platform = Platform(adapter=GeoPandasAdapter())
#         self.catalog_traffic = self.platform.get_catalog(TrafficMessage.CATALOG_HRN_TRAFFIC)
#         self.catalog_rib = self.platform.get_catalog(TrafficMessage.CATALOG_HRN_RIB)
#         self.layer_traffic = self.catalog_traffic.get_layer(TrafficMessage.LAYER_ID_TRAFFIC)
#         self.layer_geo = self.catalog_rib.get_layer(TrafficMessage.LAYER_ID_GEOMETRY)
#         self.data = None
# 
#     def _get_data(self, tile_ids, x1, y1, x2, y2):
#         traffic_df = self.layer_traffic.read_partitions(tile_ids,record_path="items", 
#                                                         columns=TrafficMessage.TRAFFIC_FIELDS)
#         traffic_df = traffic_df.explode("topology_segment.topology_segment_id")
#         traffic_df.columns = ['partition_id','topology_segment_id', 'start_offset','end_offset']
#         traffic_df['topology_segment_id'] = 'here:cm:segment:' + traffic_df['topology_segment_id'].astype(str)
#         geo_df = self.layer_geo.read_partitions(tile_ids, record_path='node')
#         geo_df_expl = geo_df[["partition_id", "segment_ref", "geometry.latitude", "geometry.longitude"]].explode("segment_ref")
#         geo_df_expl['identifier'] = geo_df_expl['segment_ref'].apply(lambda x: x.get('identifier'))
#         df_expl = geo_df_expl.drop(columns=['segment_ref'])
#         geo_df_rib  = geopandas.GeoDataFrame(df_expl, geometry=geopandas.points_from_xy(df_expl['geometry.longitude'], 
#                                           df_expl['geometry.latitude']))
#         # merge these two data frames
#         merged_df = traffic_df.merge(geo_df_rib, left_on=['partition_id',
#                                                       'topology_segment_id'],
#                                      how='inner', right_on=['partition_id', 'identifier'])
#         # drop columns not required
#         merged_df.drop(['partition_id', 'topology_segment_id', 'start_offset', 'end_offset'], axis=1, inplace=True)
#         geo_tf = geopandas.GeoDataFrame(merged_df)
#         filtered_df = geo_tf.cx[x1:x2, y1:y2]
# 
#         return filtered_df
# 
#     def get_geojson(self, tile_ids, x1, y1, x2, y2):
#         features = []
#         self.data = self._get_data(tile_ids, x1, y1, x2, y2)
#         insert_features = lambda X: features.append(
#             geojson.Feature(geometry=X['geometry'],
#                             properties=dict(name=X["identifier"])))
#         self.data.apply(insert_features, axis=1)
# 
#         return geojson.FeatureCollection(features)
# 
# traffic = TrafficMessage()
# print(traffic)
