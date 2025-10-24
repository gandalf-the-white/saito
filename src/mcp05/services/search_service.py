"""
Flight search service implementation.
"""

from typing import List, Dict, Optional, Any
from mcp05.services.serpapi_client import prepare_flight_search_params, run_search
from mcp05.utils.logging import logger


async def search_flights(
    origin: str, destination: str, outbound_date: str, return_date: Optional[str] = None
) -> List[Dict[str, str]]:
    """
    Search for flights using SerpAPI Google Flights.

    Args:
        origin: Departure airport code (e.g., ATL, JFK)
        destination: Arrival airport code (e.g., LAX, ORD)
        outbound_date: Departure date (YYYY-MM-DD)
        return_date: Return date for round trips (YYYY-MM-DD)

    Returns:
        A list of available flights with details
    """
    logger.info(
        f"Searching flights: {origin} to {destination}, dates: {outbound_date} - {return_date}"
    )
    logger.debug(
        f"Function called with: origin={origin}, destination={destination}, outbound_date={outbound_date}, return_date={return_date}"
    )

    # Prepare search parameters
    params = prepare_flight_search_params(
        origin,
        destination,
        outbound_date,
        return_date,
    )

    # Execute search
    logger.debug("Executing SerpAPI search...")
    search_results = await run_search(params)

    # Check for errors
    if "error" in search_results:
        logger.debug("Flight search error: {search_results['error']}")
        return [{"error": search_results["error"]}]

    # Process flight results
    return format_flight_results(search_results)


def format_flight_results(search_results: Dict[str, Any]) -> List[Dict[str, str]]:
    """
    Format raw flight search results into a standardized format.

    Args:
        search_results: Raw search results from SerpAPI

    Returns:
        Formatted list of flight information
    """
    best_flights = search_results.get("best_flights", [])
    logger.debug(f"Search complete. Found {len(best_flights)} best flights")

    if not best_flights:
        logger.warning("No flights found in search results")
        return []

    # Format flight data
    formatted_flights = []
    for i, flight in enumerate(best_flights):
        logger.debug(f"Processing flight {i + 1} of {len(best_flights)}")
        if not flight.get("flights") or len(flight["flights"]) == 0:
            logger.debug(f"Skipping flight {i + 1} as it has no flight segments")
            continue

        # Get first leg details
        first_leg = flight["flights"][0]
        logger.debug(
            f"Flight {i + 1} has airline: {first_leg.get('airline', 'Unknown')}, price: {flight.get('price', 'N/A')}"
        )

        # Process departure airport
        dep_airport = first_leg.get("departure_airport", {})
        if isinstance(dep_airport, dict):
            dep_name = dep_airport.get("name", "Unknown")
            dep_id = dep_airport.get("id", "???")
            dep_time = dep_airport.get("time", "N/A")
            departure_info = f"{dep_name} ({dep_id}) at {dep_time}"
        else:
            departure_info = first_leg.get("departure_time", "Unknown")

        # Process arrival airport
        arr_airport = first_leg.get("arrival_airport", {})
        if isinstance(arr_airport, dict):
            arr_name = arr_airport.get("name", "Unknown")
            arr_id = arr_airport.get("id", "???")
            arr_time = arr_airport.get("time", "N/A")
            arrival_info = f"{arr_name} ({arr_id}) at {arr_time}"
        else:
            arrival_info = first_leg.get("arrival_time", "Unknown")

        # Add formatted flight to results
        formatted_flights.append(
            {
                "airline": first_leg.get("airline", "Unknown Airline"),
                "price": str(flight.get("price", "N/A")),
                "duration": f"{flight.get('total_duration', 'N/A')} min",
                "stops": "Nonstop"
                if len(flight["flights"]) == 1
                else f"{len(flight['flights']) - 1} stop(s)",
                "departure": departure_info,
                "arrival": arrival_info,
                "travel_class": first_leg.get("travel_class", "Economy"),
                "airline_logo": first_leg.get("airline_logo", ""),
            }
        )

    logger.info(f"Returning {len(formatted_flights)} formatted flights")
    return formatted_flights
