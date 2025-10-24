"""
Pydantic schemas  for flight information.
"""

from pydantic import BaseModel
from typing import Optional


# Pydantic Models
class FlightInfo(BaseModel):
    """Flight information model."""

    airline: str
    price: str
    duration: str
    stops: str
    departure: str
    arrival: str
    travel_class: str
    airline_logo: Optional[str] = None
