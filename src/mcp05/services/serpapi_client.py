"""
SerpAPI client for flight searches.
"""

import asyncio
import json
from typing import Dict, Any, Optional

from mcp05.utils.logging import logger
from mcp05.config import SERP_API_KEY
from serpapi import GoogleSearch


async def run_search(params: Dict[str, Any]):
    """
    Run SerpAPI search asynchronously.

    Args:
        params: Parameters for the SerpAPI search

    Returns:
        Search results from SerpAPI
    """
    try:
        logger.debug(
            f"Sending SerpAPI request with params: {json.dumps(params, indent=2)}"
        )
        result = await asyncio.to_thread(lambda: GoogleSearch(params).get_dict())
        logger.debug(f"SerpAPI response received, keys: {list(result.keys())}")
        return result
    except Exception as e:
        logger.exception(f"SerAPI search error: {str(e)}")
        return {"error": str(e)}


def prepare_flight_search_params(
    origin: str, destination: str, outbound_date: str, return_date: Optional[str] = None
) -> Dict[str, Any]:
    """
    Prepare parameters for a flight search.

    Args:
        origin: Departure airport code
        destination: Arrival airport code
        outbound_date: Departure date (YYYY-MM-DD)
        return_date: Return date (YYYY-MM-DD)

    Returns:
        Dictionary of parameters for SerpAPI
    """
    params = {
        "api_key": SERP_API_KEY,
        "engine": "google_flights",
        "hl": "en",
        "gl": "us",
        "departure_id": origin.strip().upper(),
        "arrival_id": destination.strip().upper(),
        "outbound_date": outbound_date,
        "currency": "USD",
        "type": "2",
    }

    # Add return date if provided (making if a round trip)
    if return_date:
        logger.debug("Round trip detected, adding return_date and setting type=1")
        params["return_date"] = return_date
        params["type"] = "1"  # Set to round trip
    else:
        logger.debug("One-way trip detected, type=2")

    return params
