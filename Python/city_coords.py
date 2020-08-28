from geopy.geocoders import Nominatim
geolocator = Nominatim(user_agent="niche-county-comparison")

def city_coords(query):
    query = "%s, USA" % (query)
    location = geolocator.geocode(query, exactly_one=True)
    return [location.longitude, location.latitude]
